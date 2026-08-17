(ns stepper.dispatch-test
  (:require [clojure.test :refer [deftest is]]
            [stepper.dispatch :as dispatch]
            [stepper.engine :as engine]))

(deftest remote-client-roundtrip
  (let [machine {"StartAt" "Away"
                 "States" {"Away" {"Type" "Task"
                                   "Resource" "arn:m4pro:stepper:::shell:runCommand"
                                   "Arguments" {"command" "echo hi"}
                                   "Output" "{% $states.result.stdout %}"
                                   "End" true}}}
        run (future (engine/run machine {} (constantly nil)))
        task (dispatch/poll! "m4pro" 5000)]
    ;; the task reached the m4pro channel, ctx stripped
    (is (some? task))
    (is (= "arn:m4pro:stepper:::shell:runCommand" (:resource task)))
    (is (nil? (:ctx task)))
    ;; the client's answer becomes the state result
    (dispatch/complete! (:id task) {:result {"stdout" "answered remotely"}})
    (let [result (deref run 5000 ::stuck)]
      (is (= "SUCCEEDED" (:status result)))
      (is (= "answered remotely" (:output result)))))
  ;; polling with nothing queued waits the window out and returns nil
  (is (nil? (dispatch/poll! "m4pro" 100)))
  ;; an answer that comes after the await gave up is ignored
  (is (false? (dispatch/complete! "no-such-task" {:result 1}))))
