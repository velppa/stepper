(ns stepper.scheduler-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [clojure.string :as str]
            [stepper.db :as db]
            [stepper.scheduler :as scheduler])
  (:import (java.time Instant)))

(defn- wait-for
  "Value of F once it is truthy — schedules start executions in the
  background, so tests wait for the row instead of guessing a delay."
  [f]
  (loop [attempts 100]
    (or (f)
        (when (pos? attempts)
          (Thread/sleep 50)
          (recur (dec attempts))))))

(deftest next-run-rate
  (let [after (Instant/parse "2026-08-12T10:00:00Z")]
    (is (= (Instant/parse "2026-08-12T10:05:00Z")
           (scheduler/next-run "rate(5 minutes)" after)))
    (is (= (Instant/parse "2026-08-13T10:00:00Z")
           (scheduler/next-run "rate(1 day)" after)))))

(deftest next-run-cron
  (let [after (Instant/parse "2026-08-12T10:02:30Z")
        next (scheduler/next-run "*/5 * * * *" after)]
    (is (.isAfter next after))
    (is (zero? (mod (.getEpochSecond next) 300)))))

(deftest tick-fires-due-schedule
  (let [path (str (java.io.File/createTempFile "stepper" ".db"))
        ds (db/datasource path)]
    (db/migrate! ds)
    (db/create-state-machine! ds {:id "sm1" :name "tick-test"
                                  :definition (json/generate-string
                                               {"StartAt" "Done"
                                                "States" {"Done" {"Type" "Succeed"}}})})
    (db/create-schedule! ds {:id "s1" :state-machine-id "sm1"
                             :expression "rate(1 hour)"
                             :input "{\"k\": 1, \"at\": \"{% $input.time %}\"}"
                             :next-run-at "2020-01-01T00:00:00Z"})
    (let [fired (scheduler/tick! ds)]
      (is (= 1 (count fired)))
      ;; next_run_at advanced into the future, so the next tick is quiet
      (is (empty? (scheduler/tick! ds)))
      ;; the execution got the rendered event template as its input
      (let [e (wait-for #(first (db/executions ds "sm1")))]
        (is (some? e))
        (is (= 1 (get (json/parse-string (:input e)) "k")))
        (is (string? (get (json/parse-string (:input e)) "at")))
        ;; the firing history keeps the execution's SRN
        (let [f (first (db/firings ds "s1"))]
          (is (= (str "arn:localhost:stepper:::execution:tick-test:" (:name e))
                 (:execution-arn f))))))))

(deftest event-template-is-jsonata
  (let [now (Instant/parse "2026-08-12T10:00:00Z")]
    ;; $input.time is the firing time, plain values pass through
    (is (= {"at" "2026-08-12T10:00:00Z" "kind" "hourly"}
           (json/parse-string
            (scheduler/render-event "{\"at\": \"{% $input.time %}\", \"kind\": \"hourly\"}" now))))
    ;; expressions can compute from it
    (is (= {"date" "2026-08-12"}
           (json/parse-string
            (scheduler/render-event "{\"date\": \"{% $substring($input.time, 0, 10) %}\"}" now))))
    (is (= {} (json/parse-string (scheduler/render-event nil now))))))

(deftest schedule-errors-are-reported
  (is (empty? (scheduler/errors "0 * * * *" "{\"at\": \"{% $input.time %}\"}")))
  (is (empty? (scheduler/errors "rate(5 minutes)" nil)))
  (is (str/includes? (str/join (scheduler/errors "every monday" nil))
                     "is not a cron expression"))
  (is (str/includes? (str/join (scheduler/errors "0 * * * *" "not json"))
                     "event template is not valid JSON"))
  (is (str/includes? (str/join (scheduler/errors "0 * * * *" "{\"a\": \"{% $foo( %}\"}"))
                     "invalid JSONata expression")))

(deftest executions-are-pruned-to-the-limit
  (let [path (str (java.io.File/createTempFile "stepper" ".db"))
        ds (db/datasource path)]
    (db/migrate! ds)
    (db/create-state-machine! ds {:id "sm1" :name "pruned"
                                  :definition (json/generate-string
                                               {"StartAt" "Done"
                                                "States" {"Done" {"Type" "Succeed"}}})})
    (dotimes [i 5]
      (db/create-execution! ds {:id (str "e" i) :state-machine-id "sm1" :name (str "e" i)})
      (db/record-event! ds {:execution-id (str "e" i) :type "StateEntered"}))
    (db/prune-executions! ds "sm1" 2)
    (is (= ["e4" "e3"] (map :name (db/executions ds "sm1"))))
    ;; the pruned executions took their events with them
    (is (empty? (db/events ds "e0")))))
