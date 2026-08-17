(ns stepper.engine-test
  (:require [clojure.test :refer [deftest is testing]]
            [stepper.engine :as engine]
            [stepper.jsonata :as jsonata]))

(defn- run
  ([definition input] (run definition input {}))
  ([definition input variables]
   (engine/run definition input (constantly nil) :variables variables)))

(deftest jsonata-evaluates
  (is (= 3 (jsonata/evaluate "a + b" {"a" 1 "b" 2})))
  (is (= {"sum" 3} (jsonata/evaluate "{'sum': a + b}" {"a" 1 "b" 2})))
  (is (= 5 (jsonata/evaluate "$x + 2" {} {"x" 3}))))

(deftest pass-output
  (let [result (run {"StartAt" "Double"
                     "States" {"Double" {"Type" "Pass"
                                         "Output" "{% {'n': n * 2} %}"
                                         "End" true}}}
                    {"n" 21})]
    (is (= "SUCCEEDED" (:status result)))
    (is (= {"n" 42} (:output result)))))

(deftest fail-state
  (let [result (run {"StartAt" "Boom"
                     "States" {"Boom" {"Type" "Fail"
                                       "Error" "Custom.Error"
                                       "Cause" "on purpose"}}}
                    {})]
    (is (= "FAILED" (:status result)))
    (is (= "Custom.Error" (:error result)))))

(deftest task-shell-command
  (testing "successful command returns stdout"
    (let [result (run {"StartAt" "Echo"
                       "States" {"Echo" {"Type" "Task"
                                         "Resource" "arn:localhost:stepper:::shell:runCommand"
                                         "Arguments" {"command" "{% 'echo ' & $states.input.word %}"}
                                         "Output" "{% $trim($states.result.stdout) %}"
                                         "End" true}}}
                      {"word" "hello"})]
      (is (= "SUCCEEDED" (:status result)))
      (is (= "hello" (:output result)))))
  (testing "non-zero exit fails execution"
    (let [result (run {"StartAt" "Bad"
                       "States" {"Bad" {"Type" "Task"
                                        "Resource" "arn:localhost:stepper:::shell:runCommand"
                                        "Arguments" {"command" "exit 3"}
                                        "End" true}}}
                      {})]
      (is (= "FAILED" (:status result)))
      (is (= "States.TaskFailed" (:error result))))))

(deftest choice-routes
  (let [definition {"StartAt" "Check"
                    "States" {"Check" {"Type" "Choice"
                                       "Choices" [{"Condition" "{% n > 10 %}"
                                                   "Next" "Big"}]
                                       "Default" "Small"}
                              "Big" {"Type" "Pass" "Output" "big" "End" true}
                              "Small" {"Type" "Pass" "Output" "small" "End" true}}}]
    (is (= "big" (:output (run definition {"n" 11}))))
    (is (= "small" (:output (run definition {"n" 5}))))))

(deftest assign-variables
  (let [result (run {"StartAt" "Set"
                     "States" {"Set" {"Type" "Pass"
                                      "Assign" {"total" "{% n + 1 %}"}
                                      "Next" "Use"}
                               "Use" {"Type" "Pass"
                                      "Output" "{% $total * 10 %}"
                                      "End" true}}}
                    {"n" 4})]
    (is (= 50 (:output result)))))

(deftest retry-then-catch
  (testing "catch routes error output"
    (let [result (run {"StartAt" "Bad"
                       "States" {"Bad" {"Type" "Task"
                                        "Resource" "arn:localhost:stepper:::shell:runCommand"
                                        "Arguments" {"command" "exit 1"}
                                        "Retry" [{"ErrorEquals" ["States.TaskFailed"]
                                                  "MaxAttempts" 2
                                                  "IntervalSeconds" 0}]
                                        "Catch" [{"ErrorEquals" ["States.ALL"]
                                                  "Next" "Recover"}]
                                        "End" true}
                                 "Recover" {"Type" "Pass"
                                            "Output" "{% $states.input.Error %}"
                                            "End" true}}}
                      {})]
      (is (= "SUCCEEDED" (:status result)))
      (is (= "States.TaskFailed" (:output result))))))

(deftest parallel-branches
  (let [result (run {"StartAt" "Both"
                     "States" {"Both" {"Type" "Parallel"
                                       "Branches" [{"StartAt" "A"
                                                    "States" {"A" {"Type" "Pass"
                                                                   "Output" "{% n + 1 %}"
                                                                   "End" true}}}
                                                   {"StartAt" "B"
                                                    "States" {"B" {"Type" "Pass"
                                                                   "Output" "{% n * 2 %}"
                                                                   "End" true}}}]
                                       "End" true}}}
                    {"n" 10})]
    (is (= [11 20] (:output result)))))

(deftest map-items
  (let [result (run {"StartAt" "Each"
                     "States" {"Each" {"Type" "Map"
                                       "Items" "{% $states.input.xs %}"
                                       "ItemProcessor" {"StartAt" "Inc"
                                                        "States" {"Inc" {"Type" "Pass"
                                                                         "Output" "{% $ + 1 %}"
                                                                         "End" true}}}
                                       "End" true}}}
                    {"xs" [1 2 3]})]
    (is (= [2 3 4] (:output result)))))

(deftest events-are-emitted
  (let [events (atom [])]
    (engine/run {"StartAt" "Done"
                 "States" {"Done" {"Type" "Succeed"}}}
                {} #(swap! events conj %))
    (is (= ["StateEntered" "StateExited" "ExecutionSucceeded"]
           (map :type @events)))))

(deftest task-run-file
  (let [script (java.io.File/createTempFile "stepper" ".sh")]
    (spit script "echo \"file says $1\"\n")
    (let [result (run {"StartAt" "Run"
                       "States" {"Run" {"Type" "Task"
                                        "Resource" "arn:localhost:stepper:::shell:runFile"
                                        "Arguments" {"file" (.getAbsolutePath script)
                                                     "args" ["{% $states.input.word %}"]}
                                        "Output" "{% $trim($states.result.stdout) %}"
                                        "End" true}}}
                      {"word" "yes"})]
      (is (= "SUCCEEDED" (:status result)))
      (is (= "file says yes" (:output result))))))

(deftest task-claude-run-prompt
  (let [stub (java.io.File/createTempFile "claude-stub" "")]
    (spit stub "#!/bin/sh\necho \"claude got: $1 $2\"\n")
    (.setExecutable stub true)
    (testing "runs claude -p with the prompt"
      (let [result (run {"StartAt" "Ask"
                         "States" {"Ask" {"Type" "Task"
                                          "Resource" "arn:localhost:stepper:::claude:runPrompt"
                                          "Arguments" {"prompt" "{% $states.input.q %}"
                                                       "claude" (.getAbsolutePath stub)}
                                          "Output" "{% $trim($states.result.stdout) %}"
                                          "End" true}}}
                        {"q" "hello"})]
        (is (= "SUCCEEDED" (:status result)))
        (is (= "claude got: -p hello" (:output result)))))
    (testing "JSON output passes session information through"
      (let [json-stub (java.io.File/createTempFile "claude-stub" "")]
        (spit json-stub "#!/bin/sh\necho '{\"result\":\"hi\",\"session_id\":\"abc-123\",\"num_turns\":1}'\n")
        (.setExecutable json-stub true)
        (let [result (run {"StartAt" "Ask"
                           "States" {"Ask" {"Type" "Task"
                                            "Resource" "arn:localhost:stepper:::claude:runPrompt"
                                            "Arguments" {"prompt" "hello"
                                                         "claude" (.getAbsolutePath json-stub)}
                                            "Output" "{% $states.result.result & \"/\" & $states.result.session_id %}"
                                            "End" true}}}
                          {})]
          (is (= "SUCCEEDED" (:status result)))
          (is (= "hi/abc-123" (:output result))))))
    (testing "missing prompt fails the task"
      (let [result (run {"StartAt" "Ask"
                         "States" {"Ask" {"Type" "Task"
                                          "Resource" "arn:localhost:stepper:::claude:runPrompt"
                                          "Arguments" {"claude" (.getAbsolutePath stub)}
                                          "End" true}}}
                        {})]
        (is (= "FAILED" (:status result)))
        (is (= "States.TaskFailed" (:error result)))))))
