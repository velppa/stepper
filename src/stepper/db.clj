(ns stepper.db
  "SQLite persistence for state machines, executions and their event history."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def default-path
  (or (System/getenv "STEPPER_DB")
      (str (System/getProperty "user.home") "/.local/share/stepper/stepper.db")))

(defn datasource [path]
  (io/make-parents path)
  (jdbc/get-datasource {:dbtype "sqlite" :dbname path}))

(defn migrate! [ds]
  (doseq [stmt (->> (str/split (slurp (io/resource "schema.sql")) #";")
                    (map str/trim)
                    (remove str/blank?))]
    (jdbc/execute! ds [stmt])))

(def ^:private opts {:builder-fn rs/as-unqualified-kebab-maps})

(defn create-state-machine! [ds {:keys [id name definition]}]
  (jdbc/execute-one! ds ["INSERT INTO state_machine (id, name, definition) VALUES (?, ?, ?)"
                         id name definition]))

(defn state-machines [ds]
  (jdbc/execute! ds ["SELECT * FROM state_machine ORDER BY name"] opts))

(defn state-machine [ds id]
  (jdbc/execute-one! ds ["SELECT * FROM state_machine WHERE id = ?" id] opts))

(defn state-machine-by-name [ds name]
  (jdbc/execute-one! ds ["SELECT * FROM state_machine WHERE name = ?" name] opts))

(defn create-execution! [ds {:keys [id state-machine-id name input]}]
  (jdbc/execute-one! ds ["INSERT INTO execution (id, state_machine_id, name, input) VALUES (?, ?, ?, ?)"
                         id state-machine-id name input]))

(defn finish-execution! [ds id {:keys [status output error cause]}]
  (jdbc/execute-one! ds ["UPDATE execution
                          SET status = ?, output = ?, error = ?, cause = ?,
                              stopped_at = strftime('%Y-%m-%dT%H:%M:%fZ','now')
                          WHERE id = ?"
                         status output error cause id]))

(defn executions [ds state-machine-id]
  (jdbc/execute! ds ["SELECT * FROM execution WHERE state_machine_id = ? ORDER BY started_at DESC"
                     state-machine-id] opts))

(defn execution [ds id]
  (jdbc/execute-one! ds ["SELECT * FROM execution WHERE id = ?" id] opts))

(defn record-event! [ds {:keys [execution-id type state-name detail]}]
  (jdbc/execute-one! ds ["INSERT INTO execution_event (execution_id, type, state_name, detail) VALUES (?, ?, ?, ?)"
                         execution-id type state-name detail]))

(defn events [ds execution-id]
  (jdbc/execute! ds ["SELECT * FROM execution_event WHERE execution_id = ? ORDER BY id"
                     execution-id] opts))

(defn create-schedule! [ds {:keys [id state-machine-id expression input next-run-at]}]
  (jdbc/execute-one! ds ["INSERT INTO schedule (id, state_machine_id, expression, input, next_run_at) VALUES (?, ?, ?, ?, ?)"
                         id state-machine-id expression input next-run-at]))

(defn schedules
  ([ds] (jdbc/execute! ds ["SELECT * FROM schedule ORDER BY created_at"] opts))
  ([ds state-machine-id]
   (jdbc/execute! ds ["SELECT * FROM schedule WHERE state_machine_id = ? ORDER BY created_at"
                      state-machine-id] opts)))

(defn due-schedules [ds now]
  (jdbc/execute! ds ["SELECT * FROM schedule WHERE enabled = 1 AND next_run_at <= ?" now] opts))

(defn set-next-run! [ds id next-run-at]
  (jdbc/execute-one! ds ["UPDATE schedule SET next_run_at = ? WHERE id = ?" next-run-at id]))

(defn execution-by-name [ds state-machine-id name]
  (jdbc/execute-one! ds ["SELECT * FROM execution WHERE state_machine_id = ? AND name = ?"
                         state-machine-id name] opts))

(defn schedule [ds id]
  (jdbc/execute-one! ds ["SELECT * FROM schedule WHERE id = ?" id] opts))

(defn record-firing! [ds schedule-id execution-srn]
  (jdbc/execute-one! ds ["INSERT INTO firing (schedule_id, execution_srn) VALUES (?, ?)"
                         schedule-id execution-srn]))

(defn firings
  "Firing history of a schedule, newest first."
  [ds schedule-id]
  (jdbc/execute! ds ["SELECT * FROM firing WHERE schedule_id = ? ORDER BY id DESC"
                     schedule-id] opts))
