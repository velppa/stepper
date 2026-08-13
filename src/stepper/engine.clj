(ns stepper.engine
  "Amazon States Language interpreter, JSONata mode only.

  JSONata expressions appear as \"{% expr %}\" strings anywhere inside
  Arguments, Output, Assign, Condition and Fail's Error/Cause; plain
  values pass through literally.  Expressions are evaluated against the
  state input with the reserved $states variable bound to
  {input, result, errorOutput, context} plus the machine's assigned
  variables."
  (:require [clojure.string :as str]
            [stepper.jsonata :as jsonata]
            [stepper.resource :as resource]))

(defn- expression? [x]
  (and (string? x)
       (str/starts-with? x "{%")
       (str/ends-with? x "%}")))

(defn- eval-expr [x {:keys [input states variables]}]
  (jsonata/evaluate (subs x 2 (- (count x) 2))
                    input
                    (assoc variables "states" states)))

(defn- eval-template
  "Evaluate every {% %} expression inside X, leaving other values as-is."
  [x env]
  (cond
    (expression? x) (eval-expr x env)
    (map? x) (update-vals x #(eval-template % env))
    (sequential? x) (mapv #(eval-template % env) x)
    :else x))

(defn render
  "Evaluate every {% %} expression inside X with BINDINGS as variables,
  leaving other values as they are."
  [x bindings]
  (eval-template x {:input {} :states {} :variables bindings}))

(defn- states-var [input & {:as extra}]
  (merge {"input" input "context" {}} extra))

(defn- task-failure
  "Normalize a throwable into ASL error/cause."
  [e]
  (let [{:keys [error cause]} (ex-data e)]
    {:error (or error "States.TaskFailed")
     :cause (or cause (ex-message e))}))

(defn- error-matches? [error-equals error]
  (some #(or (= % "States.ALL") (= % error)) error-equals))

(defn- retry-delay
  "Seconds to sleep before ATTEMPT (1-based) under RETRIER."
  [{:strs [IntervalSeconds BackoffRate MaxDelaySeconds JitterStrategy]
    :or {IntervalSeconds 1 BackoffRate 2.0}}
   attempt]
  (let [delay (* IntervalSeconds (Math/pow BackoffRate (dec attempt)))
        delay (if MaxDelaySeconds (min delay MaxDelaySeconds) delay)]
    (if (= JitterStrategy "FULL") (rand delay) delay)))

(declare run)

(defn- run-task [state env]
  (let [arguments (eval-template (get state "Arguments" {}) env)]
    (resource/invoke (get state "Resource") arguments)))

(defn- run-choice [state env]
  (or (some (fn [{:strs [Condition Next]}]
              (when (eval-template Condition env) Next))
            (get state "Choices"))
      (get state "Default")
      (throw (ex-info "no choice matched and no Default"
                      {:error "States.NoChoiceMatched"
                       :cause "no choice rule matched"}))))

(defn- run-wait [state env]
  (let [seconds (eval-template (get state "Seconds") env)
        timestamp (eval-template (get state "Timestamp") env)
        millis (cond
                 seconds (* 1000 (long seconds))
                 timestamp (- (.toEpochMilli (java.time.Instant/parse timestamp))
                              (System/currentTimeMillis))
                 :else 0)]
    (when (pos? millis) (Thread/sleep millis))))

(defn- run-branch [definition input on-event variables]
  (let [result (run definition input on-event :variables variables)]
    (if (= "SUCCEEDED" (:status result))
      (:output result)
      (throw (ex-info "branch failed"
                      {:error (or (:error result) "States.BranchFailed")
                       :cause (:cause result)})))))

(defn- attempt-state
  "Execute one state attempt.  Returns
  {:result r} | {:next name} | {:end :succeeded/:failed ...}."
  [state input on-event variables]
  (let [env {:input input :states (states-var input) :variables variables}]
    (case (get state "Type")
      "Pass" {:result input}

      "Task" {:result (run-task state env)}

      "Choice" {:next (run-choice state env)}

      "Wait" (do (run-wait state env) {:result input})

      "Succeed" {:result input :end :succeeded}

      "Fail" {:end :failed
              :error (eval-template (get state "Error" "States.Error") env)
              :cause (eval-template (get state "Cause") env)}

      "Parallel" {:result (mapv #(run-branch % input on-event variables)
                                (get state "Branches"))}

      "Map" (let [items (eval-template (get state "Items" "{% $states.input %}") env)
                  processor (get state "ItemProcessor")]
              {:result (mapv #(run-branch processor % on-event variables) items)})

      (throw (ex-info (str "State type not implemented: " (get state "Type"))
                      {:error "States.Runtime"
                       :cause (str "unsupported state type " (get state "Type"))})))))

(defn- with-retry
  "Run THUNK under the state's Retry policy; rethrows when exhausted."
  [state on-event state-name thunk]
  (loop [attempt 1]
    (let [outcome (try {:ok (thunk)}
                       (catch Exception e {:failure (task-failure e)}))]
      (if-let [{:keys [error cause]} (:failure outcome)]
        (let [retrier (some #(when (error-matches? (get % "ErrorEquals") error) %)
                            (get state "Retry"))]
          (on-event {:type "TaskFailed" :state-name state-name
                     :detail {"error" error "cause" cause}})
          (if (and retrier (< attempt (get retrier "MaxAttempts" 3)))
            (do (Thread/sleep (long (* 1000 (retry-delay retrier attempt))))
                (recur (inc attempt)))
            (throw (ex-info "state failed" {:error error :cause cause}))))
        (:ok outcome)))))

(defn run
  "Run machine DEFINITION (parsed ASL) on INPUT, calling ON-EVENT with
  each history event.  Returns {:status \"SUCCEEDED\" :output ...} or
  {:status \"FAILED\" :error ... :cause ...}."
  [definition input on-event & {:keys [variables] :or {variables {}}}]
  (when-let [ql (get definition "QueryLanguage")]
    (assert (= ql "JSONata") "only JSONata query language is supported"))
  (loop [state-name (get definition "StartAt")
         input input
         variables variables]
    (let [state (get-in definition ["States" state-name])
          _ (when-not state
              (throw (ex-info (str "state not found: " state-name) {})))
          _ (on-event {:type "StateEntered" :state-name state-name
                       :detail {"input" input}})
          outcome (try
                    (with-retry state on-event state-name
                      #(attempt-state state input on-event variables))
                    (catch Exception e {:caught (task-failure e)}))]
      (if-let [{:keys [error cause]} (and (map? outcome) (:caught outcome))]
        ;; Route through Catch or fail the execution.
        (if-let [catcher (some #(when (error-matches? (get % "ErrorEquals") error) %)
                               (get state "Catch"))]
          (let [error-output {"Error" error "Cause" cause}]
            (on-event {:type "StateExited" :state-name state-name
                       :detail {"errorOutput" error-output}})
            (recur (get catcher "Next") error-output variables))
          (do (on-event {:type "ExecutionFailed" :state-name state-name
                         :detail {"error" error "cause" cause}})
              {:status "FAILED" :error error :cause cause}))
        (let [{:keys [result next end error cause]} outcome
              env {:input input
                   :states (states-var input "result" result)
                   :variables variables}
              output (if-let [expr (get state "Output")]
                       (eval-template expr env)
                       (if next input result))
              variables (merge variables
                               (eval-template (get state "Assign" {}) env))]
          (on-event {:type "StateExited" :state-name state-name
                     :detail {"output" output}})
          (cond
            (= end :failed)
            (do (on-event {:type "ExecutionFailed" :state-name state-name
                           :detail {"error" error "cause" cause}})
                {:status "FAILED" :error error :cause cause})

            (or (= end :succeeded) (get state "End"))
            (do (on-event {:type "ExecutionSucceeded" :state-name state-name
                           :detail {"output" output}})
                {:status "SUCCEEDED" :output output})

            :else
            (recur (or next (get state "Next")) output variables)))))))
