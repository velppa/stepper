(ns stepper.run
  "Starting and recording executions."
  (:require [cheshire.core :as json]
            [stepper.db :as db]
            [stepper.engine :as engine]))

(defn execute!
  "Run MACHINE on INPUT-JSON synchronously, recording the execution and
  its events.  Returns the engine result with :execution-id added."
  [ds machine input-json {:keys [id name]}]
  (let [execution-id (or id (str (random-uuid)))]
    (db/create-execution! ds {:id execution-id
                              :state-machine-id (:id machine)
                              :name (or name (str "run-" (System/currentTimeMillis)))
                              :input input-json})
    (let [result (try
                   (engine/run (json/parse-string (:definition machine))
                               (json/parse-string (or (not-empty input-json) "{}"))
                               (fn [{:keys [type state-name detail]}]
                                 (db/record-event! ds {:execution-id execution-id
                                                       :type type
                                                       :state-name state-name
                                                       :detail (json/generate-string detail)})))
                   (catch Exception e
                     {:status "FAILED" :error "States.Runtime" :cause (ex-message e)}))]
      (db/finish-execution! ds execution-id
                            {:status (:status result)
                             :output (some-> (:output result) json/generate-string)
                             :error (:error result)
                             :cause (:cause result)})
      (assoc result :execution-id execution-id))))

(defn execute-async!
  "Like execute!, but runs in the background; returns the execution id."
  [ds machine input-json opts]
  (let [execution-id (str (random-uuid))]
    (future (execute! ds machine input-json (assoc opts :id execution-id)))
    execution-id))
