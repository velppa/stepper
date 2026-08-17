(ns stepper.version-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [stepper.db :as db]
            [stepper.run :as run]))

(defn- definition [output]
  (json/generate-string {"StartAt" "Say"
                         "States" {"Say" {"Type" "Pass"
                                          "Output" output
                                          "End" true}}}))

(defn- fresh-db []
  (doto (db/datasource (str (java.io.File/createTempFile "stepper" ".db")))
    db/migrate!))

(deftest versions-append
  (let [ds (fresh-db)]
    (db/create-state-machine! ds {:id "sm1" :name "m" :definition (definition "one")})
    (db/add-version! ds "sm1" (definition "two"))
    (is (= [2 1] (map :version (db/versions ds "sm1"))))
    (is (= 2 (:version (db/current-version ds "sm1"))))))

(deftest execution-runs-and-pins-current-version
  (let [ds (fresh-db)
        _ (db/create-state-machine! ds {:id "sm1" :name "m" :definition (definition "one")})
        machine (db/state-machine ds "sm1")
        first-run (run/execute! ds machine "{}" {:name "e1"})
        _ (db/add-version! ds "sm1" (definition "two"))
        second-run (run/execute! ds machine "{}" {:name "e2"})]
    ;; each execution ran the definition current at its start
    (is (= "one" (:output first-run)))
    (is (= "two" (:output second-run)))
    ;; and stays pinned to that version
    (is (= 1 (->> (db/execution ds (:execution-id first-run))
                  :state-machine-version-id (db/version ds) :version)))
    (is (= 2 (->> (db/execution ds (:execution-id second-run))
                  :state-machine-version-id (db/version ds) :version)))))

(deftest execution-name-is-chosen-by-the-caller
  (let [ds (fresh-db)
        _ (db/create-state-machine! ds {:id "sm1" :name "m" :definition (definition "one")})
        machine (db/state-machine ds "sm1")]
    (run/execute! ds machine "{}" {:name "nightly"})
    (is (= "nightly" (:name (db/execution-by-name ds "sm1" "nightly"))))
    ;; the same name twice is refused, and nothing extra is recorded
    (is (thrown? clojure.lang.ExceptionInfo
                 (run/execute! ds machine "{}" {:name "nightly"})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (run/execute-async! ds machine "{}" {:name "nightly"})))
    (is (= 1 (count (db/executions ds "sm1"))))
    ;; as is a name that would not survive an ARN
    (is (thrown? clojure.lang.ExceptionInfo
                 (run/execute! ds machine "{}" {:name "has spaces"})))))

(deftest async-execution-is-visible-immediately
  (let [ds (fresh-db)
        _ (db/create-state-machine! ds {:id "sm1" :name "m" :definition (definition "one")})
        machine (db/state-machine ds "sm1")
        id (run/execute-async! ds machine "{}" {:name "eager"})]
    ;; the row exists as soon as the caller has the id, not when the
    ;; background run gets around to it
    (is (some? (db/execution ds id)))))
