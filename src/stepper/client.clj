(ns stepper.client
  "Standalone Task executor.  Long-polls the server for Tasks addressed
  to this client's name, runs them via stepper.resource, posts the
  outcomes back.  Polling doubles as the client's heartbeat."
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]
            [stepper.resource :as resource])
  (:gen-class))

(defn- outcome
  "Run TASK here; {\"result\" ...} or {\"error\" ... \"cause\" ...}."
  [{:strs [resource arguments]}]
  (try {"result" (resource/invoke resource arguments {})}
       (catch Exception e
         {"error" (or (:error (ex-data e)) "States.TaskFailed")
          "cause" (or (:cause (ex-data e)) (ex-message e))})))

(defn- auth-header []
  (when-let [token (System/getenv "STEPPER_API_TOKEN")]
    {"Authorization" (str "Bearer " token)}))

(defn- post-result! [server client task-outcome]
  (let [{:keys [status error]}
        @(http/post (str server "/client/" client "/result")
                    {:headers (merge {"Content-Type" "application/json"}
                                     (auth-header))
                     :body (json/generate-string task-outcome)})]
    (when (or error (not= 200 status))
      (println "result not delivered:" (or error status)))))

(def ^:private running
  ;; task id -> future; cancelling interrupts the task, which kills its
  ;; process (stepper.resource) and fails it with States.Aborted
  (atom {}))

(defn- start-task!
  "Run TASK in the background, posting the outcome when it finishes."
  [server client task]
  (let [id (get task "id")]
    (println "task" id "-" (get task "resource"))
    (swap! running assoc id
           (future
             (try (post-result! server client (assoc (outcome task) "id" id))
                  (finally (swap! running dissoc id)))))))

(defn- stop-task! [id]
  (println "stop" id)
  (some-> (get @running id) future-cancel))

(defn- poll-once!
  "One long-poll iteration; polling continues while tasks run, so a stop
  message can reach a task in flight."
  [server client]
  (let [{:keys [status body error]}
        @(http/get (str server "/client/" client "/poll")
                   {:timeout 40000 :as :text :headers (or (auth-header) {})})]
    (cond
      error (do (println "poll failed:" error "- retrying in 5s")
                (Thread/sleep 5000))
      (= 200 status) (let [message (json/parse-string body)]
                       (if (= "stop" (get message "type"))
                         (stop-task! (get message "id"))
                         (start-task! server client message)))
      (= 204 status) nil
      :else (do (println "unexpected poll status:" status "- retrying in 5s")
                (Thread/sleep 5000)))))

(defn -main [& [server client]]
  (let [server (or server (System/getenv "STEPPER_SERVER"))
        client (or client (System/getenv "STEPPER_CLIENT"))]
    (when-not (and server client)
      (println "usage: stepper-client <server-url> <client-name>")
      (System/exit 1))
    (println "polling" server "as" client)
    (loop []
      (poll-once! server client)
      (recur))))
