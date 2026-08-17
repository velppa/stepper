(ns stepper.dispatch
  "Routing Tasks to the clients that execute them.

  Every client has a channel; starting a Task puts it there and blocks
  on a promise until the client reports the outcome.  Remote clients
  drain their channel over HTTP long-polling (poll! / complete!);
  localhost is a worker thread inside the server draining its own
  channel.  A client's polling doubles as its heartbeat (clients)."
  (:require [clojure.core.async :as async]
            [stepper.resource :as resource]))

(defonce ^:private state
  (atom {:clients {}    ; name -> {:chan ... :last-poll ...}
         :pending {}})) ; task id -> promise

(defn- client-chan [name]
  (or (get-in @state [:clients name :chan])
      (get-in (swap! state update-in [:clients name :chan]
                     #(or % (async/chan 64)))
              [:clients name :chan])))

(defn clients
  "Known clients with their last poll time: {name inst}."
  []
  (into {} (for [[name c] (:clients @state)] [name (:last-poll c)])))

(def ^:private await-margin-ms 60000)
(def ^:private default-await-ms (* 3600 1000))

(defn- outcome->result
  "Result of OUTCOME, throwing the task failure it carries."
  [{:keys [result error cause]}]
  (if error
    (throw (ex-info "task failed" {:error error :cause cause}))
    result))

(defn execute!
  "Send a Task to the client named in its resource ARN and await the
  outcome.  Returns the task result; throws ex-info {:error :cause} on
  failure or when the client does not answer in time (the task's
  timeout_seconds plus a margin)."
  [arn arguments ctx]
  (let [client (resource/arn-client arn)
        id (str (random-uuid))
        p (promise)
        await-ms (if-let [t (get arguments "timeout_seconds")]
                   (+ (* 1000 (long t)) await-margin-ms)
                   default-await-ms)]
    (swap! state assoc-in [:pending id] p)
    (async/>!! (client-chan client)
               {:id id :resource arn :arguments arguments :ctx ctx})
    (try
      (let [outcome (deref p await-ms ::timeout)]
        (if (= ::timeout outcome)
          (throw (ex-info "client did not answer"
                          {:error "States.Timeout"
                           :cause (str "client " client " did not finish "
                                       arn " in time")}))
          (outcome->result outcome)))
      (finally (swap! state update :pending dissoc id)))))

(defn poll!
  "Next Task for CLIENT, waiting up to TIMEOUT-MS; nil when none came.
  The task is returned without its ctx - it does not cross the wire."
  [client timeout-ms]
  (swap! state assoc-in [:clients client :last-poll] (java.time.Instant/now))
  (let [[task _] (async/alts!! [(client-chan client)
                                (async/timeout timeout-ms)])]
    (some-> task (dissoc :ctx))))

(defn complete!
  "Deliver a task OUTCOME {:result ...} or {:error ... :cause ...}
  reported by a client.  Unknown ids (an answer after the await gave
  up) are ignored; returns whether the outcome landed."
  [id outcome]
  (if-let [p (get-in @state [:pending id])]
    (do (deliver p outcome) true)
    false))

(defn- run-local
  "Execute TASK in this process and complete it."
  [{:keys [id resource arguments ctx]}]
  (complete! id
             (try {:result (resource/invoke resource arguments (or ctx {}))}
                  (catch Exception e
                    {:error (or (:error (ex-data e)) "States.TaskFailed")
                     :cause (or (:cause (ex-data e)) (ex-message e))}))))

(defonce ^:private localhost-worker
  (delay (doto (Thread.
                (fn []
                  (loop []
                    (when-let [task (async/<!! (client-chan "localhost"))]
                      ;; one future per task - parallel branches must not
                      ;; queue behind a long-running localhost task
                      (future (run-local task))
                      (recur))))
                "stepper-localhost-worker")
           (.setDaemon true)
           (.start))))

(defn ensure-localhost!
  "Start the in-server worker draining the localhost channel."
  []
  @localhost-worker)
