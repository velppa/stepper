(ns stepper.run
  "Starting and recording executions."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [stepper.db :as db]
            [stepper.engine :as engine]
            [stepper.validate :as validate]))

(defn generated-name
  "Name for an execution the caller did not name, PREFIX telling where it
  was started from."
  [prefix]
  (str prefix "-" (System/currentTimeMillis)))

(defn check-name!
  "Throw ex-info {:errors [...]} unless NAME can be used for a new
  execution of MACHINE.  Callers check before starting so a background
  run cannot swallow the reason."
  [ds machine name]
  (when-let [errors (seq (concat (validate/execution-name-errors name)
                                 (when (db/execution-by-name ds (:id machine) name)
                                   [(str "execution " (pr-str name) " of " (:name machine)
                                         " already exists")])))]
    (throw (ex-info "execution not started" {:errors errors}))))

(defn- start-execution!
  "Insert the RUNNING execution row, pinned to the machine's current
  definition version.  Returns {:execution-id :version} for
  run-execution!."
  [ds machine input-json {:keys [id name]}]
  (let [execution-id (or id (str (random-uuid)))
        name (or name (generated-name "run"))
        version (db/current-version ds (:id machine))]
    (check-name! ds machine name)
    (db/create-execution! ds {:id execution-id
                              :state-machine-id (:id machine)
                              :state-machine-version-id (:id version)
                              :name name
                              :input input-json})
    {:execution-id execution-id :version version}))

(defn- run-execution!
  "Run a started execution to completion, recording events and the
  outcome.  Returns the engine result with :execution-id added."
  [ds machine input-json {:keys [execution-id version]}]
  (let [result (try
                 (engine/run (json/parse-string (:definition version))
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
    (db/prune-executions! ds (:id machine))
    (assoc result :execution-id execution-id)))

(defn execute!
  "Run MACHINE on INPUT-JSON synchronously, recording the execution and
  its events.  The execution runs the machine's current definition
  version and stays pinned to it.  Returns the engine result with
  :execution-id added."
  [ds machine input-json opts]
  (run-execution! ds machine input-json
                  (start-execution! ds machine input-json opts)))

(defn execution-arn [machine-name execution-name]
  (str "arn:localhost:stepper:::execution:" machine-name ":" execution-name))

(defn parse-execution-arn
  "{:machine-name ... :execution-name ...} from an execution ARN."
  [arn]
  (let [[machine-name execution-name] (take-last 2 (str/split arn #":"))]
    {:machine-name machine-name :execution-name execution-name}))

(defn execute-async!
  "Like execute!, but the machine runs in the background; returns the
  execution id.  The name check and the RUNNING row happen before
  returning, so an unusable name is reported to the caller and the
  execution page resolves immediately."
  [ds machine input-json opts]
  (let [{:keys [execution-id] :as started}
        (start-execution! ds machine input-json opts)]
    (future (run-execution! ds machine input-json started))
    execution-id))
