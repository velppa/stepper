(ns stepper.api
  "AWS Step Functions wire protocol (AWS JSON 1.0), so the aws CLI works
  against Stepper with --endpoint-url.

  Requests are POST / with an X-Amz-Target: AWSStepFunctions.<Action>
  header; authentication is ignored."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [stepper.db :as db]
            [stepper.run :as run]
            [stepper.scheduler :as scheduler]
            [stepper.validate :as validate]))

(def ^:private arn-prefix "arn:aws:states:local:000000000000")

(defn machine-arn [name] (str arn-prefix ":stateMachine:" name))
(defn version-arn [name version] (str arn-prefix ":stateMachine:" name ":" version))
(defn- execution-arn [machine-name execution-name]
  (str arn-prefix ":execution:" machine-name ":" execution-name))

(defn- epoch [iso]
  (when iso
    (/ (.toEpochMilli (java.time.Instant/parse iso)) 1000.0)))

(defn- api-error [type message]
  {:status 400
   :headers {"Content-Type" "application/x-amz-json-1.0"}
   :body (json/generate-string {"__type" (str "com.amazonaws.swf.service.v2.model#" type)
                                "message" message})})

(defn- machine-by-arn [ds arn]
  (db/state-machine-by-name ds (last (str/split arn #":"))))

(defn- execution-by-arn [ds arn]
  (let [[machine-name execution-name] (take-last 2 (str/split arn #":"))
        machine (db/state-machine-by-name ds machine-name)]
    (when machine
      (when-let [e (db/execution-by-name ds (:id machine) execution-name)]
        (assoc e
               :machine machine
               :version (some->> (:state-machine-version-id e) (db/version ds)))))))

(defn- describe-execution [{:keys [machine version] :as e}]
  (cond-> {"executionArn" (execution-arn (:name machine) (:name e))
           "stateMachineArn" (machine-arn (:name machine))
           "name" (:name e)
           "status" (:status e)
           "startDate" (epoch (:started-at e))
           "input" (:input e)}
    version (assoc "stateMachineVersionArn" (version-arn (:name machine) (:version version)))
    (:stopped-at e) (assoc "stopDate" (epoch (:stopped-at e)))
    (:output e) (assoc "output" (:output e))
    (:error e) (assoc "error" (:error e) "cause" (:cause e))))

(defmulti action (fn [target _ds _params] target))

(defmethod action :default [target _ds _params]
  (api-error "InvalidAction" (str "unsupported action " target)))

(defn- invalid-definition [errors]
  (api-error "InvalidDefinition" (str/join "; " errors)))

(defmethod action "CreateStateMachine" [_ ds {:strs [name definition]}]
  (if-let [errors (seq (validate/errors definition))]
    (invalid-definition errors)
    (do (db/create-state-machine! ds {:id (str (random-uuid))
                                      :name name
                                      :definition definition})
        {"stateMachineArn" (machine-arn name)
         "creationDate" (epoch (str (java.time.Instant/now)))})))

(defmethod action "ListStateMachines" [_ ds _]
  {"stateMachines"
   (for [m (db/state-machines ds)]
     {"stateMachineArn" (machine-arn (:name m))
      "name" (:name m)
      "type" "STANDARD"
      "creationDate" (epoch (:created-at m))})})

(defmethod action "DescribeStateMachine" [_ ds {:strs [stateMachineArn]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    (let [v (db/current-version ds (:id m))]
      {"stateMachineArn" stateMachineArn
       "name" (:name m)
       "definition" (:definition v)
       "revisionId" (str (:version v))
       "status" "ACTIVE"
       "type" "STANDARD"
       "creationDate" (epoch (:created-at m))})
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

(defmethod action "UpdateStateMachine" [_ ds {:strs [stateMachineArn definition]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    (if-let [errors (seq (validate/errors definition))]
      (invalid-definition errors)
      (let [v (db/add-version! ds (:id m) definition)]
        {"updateDate" (epoch (str (java.time.Instant/now)))
         "revisionId" (str (:version v))
         "stateMachineVersionArn" (version-arn (:name m) (:version v))}))
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

(defmethod action "DeleteStateMachine" [_ ds {:strs [stateMachineArn]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    (do (db/delete-state-machine! ds (:id m)) {})
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

(defmethod action "ListStateMachineVersions" [_ ds {:strs [stateMachineArn]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    {"stateMachineVersions"
     (for [v (db/versions ds (:id m))]
       {"stateMachineVersionArn" (version-arn (:name m) (:version v))
        "creationDate" (epoch (:created-at v))})}
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

(defmethod action "StartExecution" [_ ds {:strs [stateMachineArn name input]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    (let [execution-name (or name (run/generated-name "api"))]
      (try
        (run/execute-async! ds m (or input "{}") {:name execution-name})
        {"executionArn" (execution-arn (:name m) execution-name)
         "startDate" (epoch (str (java.time.Instant/now)))}
        (catch clojure.lang.ExceptionInfo e
          (api-error (if (db/execution-by-name ds (:id m) execution-name)
                       "ExecutionAlreadyExists"
                       "InvalidName")
                     (str/join "; " (:errors (ex-data e)))))))
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

(defmethod action "DescribeExecution" [_ ds {:strs [executionArn]}]
  (if-let [e (execution-by-arn ds executionArn)]
    (describe-execution e)
    (api-error "ExecutionDoesNotExist" executionArn)))

(defmethod action "StopExecution" [_ ds {:strs [executionArn]}]
  (if-let [e (execution-by-arn ds executionArn)]
    (do (run/stop-execution! ds (:id e))
        {"stopDate" (epoch (str (java.time.Instant/now)))})
    (api-error "ExecutionDoesNotExist" executionArn)))

(defmethod action "ListExecutions" [_ ds {:strs [stateMachineArn]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    {"executions"
     (for [e (db/executions ds (:id m))]
       (describe-execution (assoc e
                                  :machine m
                                  :version (some->> (:state-machine-version-id e)
                                                    (db/version ds)))))}
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

;; Schedules are Stepper's own concept — Step Functions has none — so
;; these actions are named after it rather than after an AWS API.
(defn- describe-schedule [ds s]
  {"scheduleId" (:id s)
   "stateMachineArn" (machine-arn (:name (db/state-machine ds (:state-machine-id s))))
   "expression" (:expression s)
   "eventTemplate" (:input s)
   "enabled" (= 1 (:enabled s))
   "nextRunAt" (:next-run-at s)})

(defn- schedule-params
  "Fields a schedule is created or updated with, defaulting to the
  current ones when a field is left out."
  [{:strs [expression eventTemplate enabled]} current]
  {:expression (or expression (:expression current))
   :input (or eventTemplate (:input current))
   :enabled (if (some? enabled) enabled (= 1 (:enabled current)))})

(defmethod action "ListSchedules" [_ ds _]
  {"schedules" (map #(describe-schedule ds %) (db/schedules ds))})

(defmethod action "CreateSchedule" [_ ds {:strs [stateMachineArn] :as params}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    (let [{:keys [expression input enabled]} (schedule-params params nil)]
      (if-let [errors (seq (scheduler/errors expression input))]
        (api-error "InvalidSchedule" (str/join "; " errors))
        (let [id (str (random-uuid))]
          (db/create-schedule! ds {:id id
                                   :state-machine-id (:id m)
                                   :expression expression
                                   :input input
                                   :next-run-at (str (scheduler/next-run
                                                      expression (java.time.Instant/now)))})
          (when-not enabled (db/set-enabled! ds id false))
          (describe-schedule ds (db/schedule ds id)))))
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

(defmethod action "UpdateSchedule" [_ ds {:strs [scheduleId] :as params}]
  (if-let [current (db/schedule ds scheduleId)]
    (let [{:keys [expression input enabled]} (schedule-params params current)]
      (if-let [errors (seq (scheduler/errors expression input))]
        (api-error "InvalidSchedule" (str/join "; " errors))
        (do (db/update-schedule! ds scheduleId
                                 {:expression expression
                                  :input input
                                  :enabled enabled
                                  :next-run-at (str (scheduler/next-run
                                                     expression (java.time.Instant/now)))})
            (describe-schedule ds (db/schedule ds scheduleId)))))
    (api-error "ScheduleDoesNotExist" scheduleId)))

(defmethod action "DeleteSchedule" [_ ds {:strs [scheduleId]}]
  (if (db/schedule ds scheduleId)
    (do (db/delete-schedule! ds scheduleId) {})
    (api-error "ScheduleDoesNotExist" scheduleId)))

(defn- event-details
  "SFN-shaped details key for an event, so the aws CLI displays them."
  [{:keys [type state-name detail]}]
  (let [d (some-> detail json/parse-string)
        json-str #(some-> (get d %) json/generate-string)]
    (case type
      "StateEntered" {"stateEnteredEventDetails"
                      {"name" state-name "input" (json-str "input")}}
      "StateExited" {"stateExitedEventDetails"
                     {"name" state-name "output" (json-str "output")}}
      "ExecutionSucceeded" {"executionSucceededEventDetails"
                            {"output" (json-str "output")}}
      "ExecutionFailed" {"executionFailedEventDetails"
                         {"error" (get d "error") "cause" (get d "cause")}}
      "TaskFailed" {"taskFailedEventDetails"
                    {"error" (get d "error") "cause" (get d "cause")}}
      {})))

(defmethod action "GetExecutionHistory" [_ ds {:strs [executionArn]}]
  (if-let [e (execution-by-arn ds executionArn)]
    (let [events (db/events ds (:id e))]
      {"events"
       (map (fn [previous ev]
              (merge {"id" (:id ev)
                      "previousEventId" (or (:id previous) 0)
                      "timestamp" (epoch (:created-at ev))
                      "type" (:type ev)}
                     (event-details ev)))
            (cons nil events) events)})
    (api-error "ExecutionDoesNotExist" executionArn)))

(defn handle
  "Handle an AWS JSON 1.0 request; nil when the request is not one."
  [ds request]
  (when-let [target (get-in request [:headers "x-amz-target"])]
    (let [action-name (last (str/split target #"\."))
          params (try (json/parse-string (slurp (or (:body request) "")))
                      (catch Exception e
                        {::bad-request (ex-message e)}))
          result (if-let [message (::bad-request params)]
                   (api-error "InvalidParameterValue" (str "request body is not JSON — " message))
                   (try (action action-name ds (or params {}))
                        (catch Exception e
                          (api-error "InternalError"
                                     (str/join "; " (or (:errors (ex-data e))
                                                        [(ex-message e)]))))))]
      (if (:status result)
        result
        {:status 200
         :headers {"Content-Type" "application/x-amz-json-1.0"}
         :body (json/generate-string result)}))))
