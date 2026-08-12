(ns stepper.core
  "CLI entry point."
  (:require [cheshire.core :as json]
            [stepper.db :as db]
            [stepper.run :as run]
            [stepper.scheduler :as scheduler]
            [stepper.validate :as validate]
            [stepper.web :as web])
  (:gen-class))

(defn- ds []
  (doto (db/datasource db/default-path) db/migrate!))

(defn- machine [ds name]
  (or (db/state-machine-by-name ds name)
      (throw (ex-info "state machine not found" {:name name}))))

(defn- check
  "Print what is wrong with DEFINITION and exit, or return it."
  [definition]
  (when-let [errors (seq (validate/errors definition))]
    (println "invalid definition:")
    (doseq [e errors] (println " -" e))
    (System/exit 1))
  definition)

(defn- create [name definition-file]
  (db/create-state-machine! (ds) {:id (str (random-uuid))
                                  :name name
                                  :definition (check (slurp definition-file))})
  (println "created" name))

(defn- update-definition [name definition-file]
  (let [ds (ds)
        v (db/add-version! ds (:id (machine ds name)) (check (slurp definition-file)))]
    (println "updated" name "- version" (:version v))))

(defn- versions [name]
  (let [ds (ds)]
    (doseq [v (db/versions ds (:id (machine ds name)))]
      (println (:version v) "|" (:created-at v)))))

(defn- start [machine-name input-json execution-name]
  (let [ds (ds)
        result (try
                 (run/execute! ds (machine ds machine-name) input-json
                               {:name (or execution-name (run/generated-name "cli"))})
                 (catch clojure.lang.ExceptionInfo e
                   (if-let [errors (:errors (ex-data e))]
                     (do (doseq [msg errors] (println msg))
                         (System/exit 1))
                     (throw e))))]
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
    "update" (apply update-definition args)
    "versions" (versions (first args))
    "start" (start (first args) (second args) (nth args 2 nil))
    "list" (list-machines)
    "schedule" (schedule (first args) (second args) (nth args 2 nil))
    "schedules" (schedules)
    "serve" (serve (parse-long (or (first args) "8321")))
    (do (println "usage: stepper {create <name> <definition.json> | update <name> <definition.json> |
                versions <name> | start <name> [input-json] [execution-name] | list |
                schedule <name> <expression> [input-json] | schedules | serve [port]}")
        (System/exit 1)))
  (shutdown-agents))
