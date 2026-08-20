(ns stepper.termination-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [stepper.db :as db]
            [stepper.dispatch :as dispatch]
            [stepper.engine :as engine]
            [stepper.run :as run]))

(defn- fresh-db []
  (doto (db/datasource (str (java.io.File/createTempFile "stepper" ".db")))
    db/migrate!))

(deftest engine-aborts-between-states
  (let [events (atom [])
        result (engine/run {"StartAt" "Say"
                            "States" {"Say" {"Type" "Pass" "End" true}}}
                           {} #(swap! events conj %)
                           :aborted? (constantly true))]
    (is (= "ABORTED" (:status result)))
    (is (= ["ExecutionAborted"] (map :type @events)))))

(deftest abort-releases-inflight-task-and-signals-client
  (let [machine {"StartAt" "Away"
                 "States" {"Away" {"Type" "Task"
                                   "Resource" "arn:stopper:stepper:::shell:runCommand"
                                   "Arguments" {"command" "sleep 60"}
                                   "Retry" [{"ErrorEquals" ["States.ALL"]}]
                                   "End" true}}}
        running (future (engine/run machine {} (constantly nil)
                                    :execution-id "e-stop"))
        task (dispatch/poll! "stopper" 5000)]
    (is (some? task))
    (dispatch/abort-execution! "e-stop")
    ;; despite the States.ALL retrier the run ends ABORTED, immediately
    (is (= "ABORTED" (:status (deref running 2000 ::stuck))))
    ;; the stop reached the client's channel
    (let [message (dispatch/poll! "stopper" 1000)]
      (is (= "stop" (:type message)))
      (is (= (:id task) (:id message))))))

(deftest stop-execution-aborts-local-run
  (let [ds (fresh-db)
        _ (db/create-state-machine!
           ds {:id "sm1" :name "m"
               :definition (json/generate-string
                            {"StartAt" "Sleep"
                             "States" {"Sleep" {"Type" "Task"
                                                "Resource" "arn:localhost:stepper:::shell:runCommand"
                                                "Arguments" {"command" "sleep 60"}
                                                "End" true}}})})
        machine (db/state-machine ds "sm1")
        id (run/execute-async! ds machine "{}" {:name "e1"})]
    ;; wait for the task to be in flight, then stop it
    (Thread/sleep 500)
    (is (true? (run/stop-execution! ds id)))
    (loop [n 0]
      (when (and (< n 40) (= "RUNNING" (:status (db/execution ds id))))
        (Thread/sleep 100)
        (recur (inc n))))
    (let [e (db/execution ds id)]
      (is (= "ABORTED" (:status e)))
      (is (some? (:stopped-at e))))
    ;; stopping a finished execution is a no-op
    (is (nil? (run/stop-execution! ds id)))))

(deftest stop-execution-closes-orphaned-running-row
  (let [ds (fresh-db)
        _ (db/create-state-machine!
           ds {:id "sm1" :name "m"
               :definition (json/generate-string
                            {"StartAt" "Say"
                             "States" {"Say" {"Type" "Pass" "End" true}}})})
        _ (db/create-execution! ds {:id "orphan" :state-machine-id "sm1"
                                    :state-machine-version-id nil
                                    :name "orphan" :input "{}"})]
    (is (true? (run/stop-execution! ds "orphan")))
    (is (= "ABORTED" (:status (db/execution ds "orphan"))))))
