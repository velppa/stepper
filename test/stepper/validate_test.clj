(ns stepper.validate-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [stepper.db :as db]
            [stepper.validate :as validate]))

(defn- errors [definition]
  (validate/errors (if (string? definition) definition (json/generate-string definition))))

(defn- error-text [definition]
  (str/join " " (errors definition)))

(deftest valid-definition-has-no-errors
  (is (empty? (errors {"QueryLanguage" "JSONata"
                       "StartAt" "Run"
                       "States" {"Run" {"Type" "Task"
                                        "Resource" "srn:local:shell:::shell:runCommand"
                                        "Arguments" {"command" "{% 'echo ' & $states.input.x %}"}
                                        "Next" "Done"}
                                 "Done" {"Type" "Succeed"}}}))))

(deftest unparsable-json
  (is (str/includes? (error-text "not json at all") "not valid JSON")))

(deftest not-an-object
  (is (= ["definition must be a JSON object"] (errors "[1, 2]"))))

(deftest missing-and-dangling-states
  (is (str/includes? (error-text {"States" {"A" {"Type" "Succeed"}}}) "StartAt is missing"))
  (is (str/includes? (error-text {"StartAt" "Nope" "States" {"A" {"Type" "Succeed"}}})
                     "is not one of the States"))
  (is (str/includes? (error-text {"StartAt" "A"
                                  "States" {"A" {"Type" "Pass" "Next" "Ghost"}}})
                     "points at unknown state \"Ghost\""))
  (is (str/includes? (error-text {"StartAt" "A" "States" {}}) "States is empty")))

(deftest state-shape
  (is (str/includes? (error-text {"StartAt" "A" "States" {"A" {"Type" "Nonsense"}}})
                     "unsupported Type"))
  (is (str/includes? (error-text {"StartAt" "A" "States" {"A" {"Type" "Pass"}}})
                     "needs either Next or"))
  (is (str/includes? (error-text {"StartAt" "A" "States" {"A" {"Type" "Task" "End" true}}})
                     "has no Resource"))
  (is (str/includes? (error-text {"StartAt" "A" "States" {"A" {"Type" "Map" "End" true}}})
                     "has no ItemProcessor")))

(deftest bad-jsonata-expression
  (is (str/includes? (error-text {"StartAt" "A"
                                  "States" {"A" {"Type" "Pass"
                                                 "Output" "{% $foo( %}"
                                                 "End" true}}})
                     "invalid JSONata expression")))

(deftest wrong-query-language
  (is (str/includes? (error-text {"QueryLanguage" "JSONPath"
                                  "StartAt" "A"
                                  "States" {"A" {"Type" "Succeed"}}})
                     "only \"JSONata\"")))

(deftest invalid-definition-is-never-stored
  (let [ds (doto (db/datasource (str (java.io.File/createTempFile "stepper" ".db")))
             db/migrate!)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (db/create-state-machine! ds {:id "sm1" :name "m" :definition "not json"})))
    (is (empty? (db/state-machines ds)))))

(deftest execution-names
  (is (empty? (validate/execution-name-errors "nightly-2026-08-12")))
  (is (str/includes? (str/join (validate/execution-name-errors "")) "is empty"))
  (is (str/includes? (str/join (validate/execution-name-errors "has spaces")) "whitespace"))
  (is (str/includes? (str/join (validate/execution-name-errors "a/b")) "whitespace"))
  (is (str/includes? (str/join (validate/execution-name-errors (apply str (repeat 81 "x"))))
                     "longer than 80")))
