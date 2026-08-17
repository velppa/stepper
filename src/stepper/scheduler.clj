(ns stepper.scheduler
  "Starts state machines on a schedule.

  Expressions: 5-field UNIX cron (\"*/5 * * * *\") or
  rate(N seconds|minutes|hours|days).

  A schedule carries an event template — a JSON document whose {% %}
  expressions are evaluated at fire time and sent to the state machine
  as its input.  $input.time is the firing time (ISO-8601)."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [stepper.db :as db]
            [stepper.engine :as engine]
            [stepper.run :as run]
            [stepper.validate :as validate])
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

(defn render-event
  "The input a schedule sends at fire time: its event TEMPLATE with
  $input.time bound to NOW."
  [template now]
  (json/generate-string
   (engine/render (json/parse-string (or (not-empty template) "{}"))
                  {"input" {"time" (str now)}})))

(defn errors
  "Errors of a schedule's EXPRESSION and event TEMPLATE, empty when both
  are usable."
  [expression template]
  (concat
   (try (next-run expression (Instant/now)) nil
        (catch Exception e
          [(str (pr-str expression)
                " is not a cron expression or rate(N seconds|minutes|hours|days)"
                " — " (ex-message e))]))
   (when (not-empty template)
     (let [parsed (try (json/parse-string template)
                       (catch Exception e {::unparsable (ex-message e)}))]
       (if-let [message (::unparsable parsed)]
         [(str "event template is not valid JSON — " (first (str/split-lines message)))]
         (validate/expression-errors "event template" parsed))))))

(defn- fire! [ds {:keys [id state-machine-id expression input]}]
  (let [now (Instant/now)
        machine (db/state-machine ds state-machine-id)
        event (render-event input now)
        execution-name (run/generated-name "schedule")]
    (db/set-next-run! ds id (str (next-run expression now)))
    (db/record-firing! ds id (run/execution-arn (:name machine) execution-name))
    (run/execute-async! ds machine event {:name execution-name})))

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
