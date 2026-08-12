(ns stepper.scheduler-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [stepper.db :as db]
            [stepper.scheduler :as scheduler])
  (:import (java.time Instant)))

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
                             :input "{\"k\": 1}"
                             :next-run-at "2020-01-01T00:00:00Z"})
    (let [fired (scheduler/tick! ds)]
      (is (= 1 (count fired)))
      ;; next_run_at advanced into the future, so the next tick is quiet
      (is (empty? (scheduler/tick! ds)))
      ;; the execution got the fire time merged into its input
      (Thread/sleep 500)
      (let [e (first (db/executions ds "sm1"))]
        (is (some? e))
        (is (= 1 (get (json/parse-string (:input e)) "k")))
        (is (contains? (json/parse-string (:input e)) "time"))
        ;; the firing history keeps the execution's SRN
        (let [f (first (db/firings ds "s1"))]
          (is (= (str "srn:local:states:::execution:tick-test:" (:name e))
                 (:execution-srn f))))))))
