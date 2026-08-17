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

(defn- poll-once!
  "One long-poll iteration; runs the task when one arrived."
  [server client]
  (let [{:keys [status body error]}
        @(http/get (str server "/client/" client "/poll")
                   {:timeout 40000 :as :text :headers (or (auth-header) {})})]
    (cond
      error (do (println "poll failed:" error "- retrying in 5s")
                (Thread/sleep 5000))
      (= 200 status) (let [task (json/parse-string body)]
                       (println "task" (get task "id") "-" (get task "resource"))
                       (post-result! server client
                                     (assoc (outcome task) "id" (get task "id"))))
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
