(ns stepper.api
  "AWS Step Functions wire protocol (AWS JSON 1.0), so the aws CLI works
  against Stepper with --endpoint-url.

  Requests are POST / with an X-Amz-Target: AWSStepFunctions.<Action>
  header; authentication is ignored."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [stepper.db :as db]
            [stepper.run :as run]))

(def ^:private arn-prefix "arn:aws:states:local:000000000000")

(defn- machine-arn [name] (str arn-prefix ":stateMachine:" name))
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
      (some-> (db/execution-by-name ds (:id machine) execution-name)
              (assoc :machine machine)))))

(defn- describe-execution [{:keys [machine] :as e}]
  (cond-> {"executionArn" (execution-arn (:name machine) (:name e))
           "stateMachineArn" (machine-arn (:name machine))
           "name" (:name e)
           "status" (:status e)
           "startDate" (epoch (:started-at e))
           "input" (:input e)}
    (:stopped-at e) (assoc "stopDate" (epoch (:stopped-at e)))
    (:output e) (assoc "output" (:output e))
    (:error e) (assoc "error" (:error e) "cause" (:cause e))))

(defmulti action (fn [target _ds _params] target))

(defmethod action :default [target _ds _params]
  (api-error "InvalidAction" (str "unsupported action " target)))

(defmethod action "CreateStateMachine" [_ ds {:strs [name definition]}]
  (db/create-state-machine! ds {:id (str (random-uuid))
                                :name name
                                :definition definition})
  {"stateMachineArn" (machine-arn name)
   "creationDate" (epoch (str (java.time.Instant/now)))})

(defmethod action "ListStateMachines" [_ ds _]
  {"stateMachines"
   (for [m (db/state-machines ds)]
     {"stateMachineArn" (machine-arn (:name m))
      "name" (:name m)
      "type" "STANDARD"
      "creationDate" (epoch (:created-at m))})})

(defmethod action "DescribeStateMachine" [_ ds {:strs [stateMachineArn]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    {"stateMachineArn" stateMachineArn
     "name" (:name m)
     "definition" (:definition m)
     "status" "ACTIVE"
     "type" "STANDARD"
     "creationDate" (epoch (:created-at m))}
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

(defmethod action "StartExecution" [_ ds {:strs [stateMachineArn name input]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    (let [execution-name (or name (str "api-" (System/currentTimeMillis)))]
      (run/execute-async! ds m (or input "{}") {:name execution-name})
      {"executionArn" (execution-arn (:name m) execution-name)
       "startDate" (epoch (str (java.time.Instant/now)))})
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

(defmethod action "DescribeExecution" [_ ds {:strs [executionArn]}]
  (if-let [e (execution-by-arn ds executionArn)]
    (describe-execution e)
    (api-error "ExecutionDoesNotExist" executionArn)))

(defmethod action "ListExecutions" [_ ds {:strs [stateMachineArn]}]
  (if-let [m (machine-by-arn ds stateMachineArn)]
    {"executions"
     (for [e (db/executions ds (:id m))]
       (describe-execution (assoc e :machine m)))}
    (api-error "StateMachineDoesNotExist" stateMachineArn)))

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
          params (json/parse-string (slurp (or (:body request) "")))
          result (action action-name ds (or params {}))]
      (if (:status result)
        result
        {:status 200
         :headers {"Content-Type" "application/x-amz-json-1.0"}
         :body (json/generate-string result)}))))
