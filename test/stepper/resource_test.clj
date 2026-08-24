(ns stepper.resource-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [stepper.resource :as resource]))

(defn- stub-claude!
  "A fake claude binary: an executable script that ignores its
  arguments and prints a canned --output-format json reply."
  [reply-text]
  (let [f (java.io.File/createTempFile "stub-claude" ".sh")]
    (spit f (str "#!/bin/sh\ncat <<'EOF'\n"
                 (json/generate-string {"result" reply-text})
                 "\nEOF\n"))
    (.setExecutable f true)
    (.getAbsolutePath f)))

(deftest run-prompt-succeeds-on-a-plain-answer
  (let [result (resource/invoke "arn:localhost:stepper:::claude:runPrompt"
                                {"prompt" "hi" "claude" (stub-claude! "all done.")}
                                {})]
    (is (= "all done." (get result "result")))))

(deftest run-prompt-fails-when-the-reply-ends-in-a-question
  (try
    (resource/invoke "arn:localhost:stepper:::claude:runPrompt"
                     {"prompt" "hi" "claude" (stub-claude! "should I proceed?")}
                     {})
    (is false "expected invoke to throw")
    (catch clojure.lang.ExceptionInfo e
      (is (= "Claude.UnansweredQuestion" (:error (ex-data e))))
      (is (= "should I proceed?" (get-in (ex-data e) [:result "result"]))))))

(deftest run-prompt-ignores-a-question-mark-mid-reply
  (let [result (resource/invoke "arn:localhost:stepper:::claude:runPrompt"
                                {"prompt" "hi" "claude" (stub-claude! "why? because it works.")}
                                {})]
    (is (= "why? because it works." (get result "result")))))
