(ns stepper.scheduler
  "Starts state machines on a schedule.

  Expressions: 5-field UNIX cron (\"*/5 * * * *\") or
  rate(N seconds|minutes|hours|days).  The fire time is merged into the
  execution input as \"time\" (ISO-8601)."
  (:require [cheshire.core :as json]
            [stepper.db :as db]
            [stepper.run :as run])
  (:import (com.cronutils.model CronType)
           (com.cronutils.model.definition CronDefinitionBuilder)
           (com.cronutils.model.time ExecutionTime)
           (com.cronutils.parser CronParser)
           (java.time Instant ZonedDateTime ZoneId)
           (java.time.temporal ChronoUnit)))

(def ^:private cron-parser
  (CronParser. (CronDefinitionBuilder/instanceDefinitionFor CronType/UNIX)))

(defn next-run
  "Next fire Instant for EXPRESSION strictly after Instant AFTER."
  [expression after]
  (if-let [[_ n unit] (re-matches #"rate\((\d+) (second|minute|hour|day)s?\)" expression)]
    (.plus ^Instant after (Long/parseLong n)
           (case unit
             "second" ChronoUnit/SECONDS
             "minute" ChronoUnit/MINUTES
             "hour" ChronoUnit/HOURS
             "day" ChronoUnit/DAYS))
    (let [cron (.parse cron-parser expression)
          zoned (ZonedDateTime/ofInstant after (ZoneId/systemDefault))]
      (-> (ExecutionTime/forCron cron)
          (.nextExecution zoned)
          (.orElseThrow)
          (.toInstant)))))

(defn- fire! [ds {:keys [id state-machine-id expression input]}]
  (let [now (Instant/now)
        machine (db/state-machine ds state-machine-id)
        merged (-> (json/parse-string (or (not-empty input) "{}"))
                   (assoc "time" (str now))
                   json/generate-string)
        execution-name (str "schedule-" (System/currentTimeMillis))]
    (db/set-next-run! ds id (str (next-run expression now)))
    (db/record-firing! ds id (run/execution-srn (:name machine) execution-name))
    (run/execute-async! ds machine merged {:name execution-name})))

(defn tick!
  "Fire every due schedule once; returns the fired schedules."
  [ds]
  (let [due (db/due-schedules ds (str (Instant/now)))]
    (doseq [s due] (fire! ds s))
    due))

(defn start!
  "Run the scheduler loop in a daemon thread, ticking every 10s."
  [ds]
  (doto (Thread.
         (fn []
           (loop []
             (try (tick! ds)
                  (catch Exception e
                    (println "scheduler error:" (ex-message e))))
             (Thread/sleep 10000)
             (recur))))
    (.setDaemon true)
    (.setName "stepper-scheduler")
    (.start)))
