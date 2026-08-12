(ns stepper.core
  "CLI entry point."
  (:require [cheshire.core :as json]
            [stepper.db :as db]
            [stepper.run :as run]
            [stepper.scheduler :as scheduler]
            [stepper.web :as web])
  (:gen-class))

(defn- ds []
  (doto (db/datasource db/default-path) db/migrate!))

(defn- machine [ds name]
  (or (db/state-machine-by-name ds name)
      (throw (ex-info "state machine not found" {:name name}))))

(defn- create [name definition-file]
  (db/create-state-machine! (ds) {:id (str (random-uuid))
                                  :name name
                                  :definition (slurp definition-file)})
  (println "created" name))

(defn- start [machine-name input-json]
  (let [ds (ds)
        result (run/execute! ds (machine ds machine-name) input-json
                             {:name (str "cli-" (System/currentTimeMillis))})]
    (println (:status result))
    (some-> (:output result) json/generate-string println)))

(defn- list-machines []
  (doseq [m (db/state-machines (ds))]
    (println (:name m))))

(defn- schedule [machine-name expression input-json]
  (let [ds (ds)
        m (machine ds machine-name)
        next (scheduler/next-run expression (java.time.Instant/now))]
    (db/create-schedule! ds {:id (str (random-uuid))
                             :state-machine-id (:id m)
                             :expression expression
                             :input input-json
                             :next-run-at (str next)})
    (println "scheduled" machine-name "-" expression "- next run" (str next))))

(defn- schedules []
  (let [ds (ds)]
    (doseq [s (db/schedules ds)]
      (println (:name (db/state-machine ds (:state-machine-id s)))
               "|" (:expression s)
               "| next" (:next-run-at s)
               "|" (if (= 1 (:enabled s)) "enabled" "disabled")))))

(defn- serve [port]
  (let [ds (ds)]
    (scheduler/start! ds)
    (web/serve ds port)))

(defn -main [& [cmd & args]]
  (case cmd
    "create" (apply create args)
    "start" (start (first args) (second args))
    "list" (list-machines)
    "schedule" (schedule (first args) (second args) (nth args 2 nil))
    "schedules" (schedules)
    "serve" (serve (parse-long (or (first args) "8321")))
    (do (println "usage: stepper {create <name> <definition.json> | start <name> [input-json] |
                list | schedule <name> <expression> [input-json] | schedules | serve [port]}")
        (System/exit 1)))
  (shutdown-agents))
