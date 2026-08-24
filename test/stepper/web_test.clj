(ns stepper.web-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [stepper.db :as db]
            [stepper.run]
            [stepper.web]))

(def ^:private route #'stepper.web/route)

(defn- fresh-db []
  (doto (db/datasource (str (java.io.File/createTempFile "stepper" ".db")))
    db/migrate!))

(defn- post [handler uri params]
  (handler {:uri uri
            :request-method :post
            :body (-> (str/join "&" (for [[k v] params]
                                      (str k "=" (java.net.URLEncoder/encode (str v) "UTF-8"))))
                      (.getBytes "UTF-8")
                      java.io.ByteArrayInputStream.)}))

(def ^:private definition
  (json/generate-string {"StartAt" "Say"
                         "States" {"Say" {"Type" "Pass" "Output" "hi" "End" true}}}))

(deftest create-machine-via-web
  (let [ds (fresh-db)
        handler (route ds)
        resp (post handler "/machine" {"name" "hello" "definition" definition})]
    (is (= 303 (:status resp)))
    (is (= "/machine/hello" (get-in resp [:headers "Location"])))
    (is (= 1 (->> (db/state-machine-by-name ds "hello") :id
                  (db/current-version ds) :version)))))

(deftest stop-route-aborts-running-execution
  (let [ds (fresh-db)
        handler (route ds)
        _ (db/create-state-machine!
           ds {:id "sm1" :name "m"
               :definition (json/generate-string
                            {"StartAt" "Sleep"
                             "States" {"Sleep" {"Type" "Task"
                                                "Resource" "arn:localhost:stepper:::shell:runCommand"
                                                "Arguments" {"command" "sleep 60"}
                                                "End" true}}})})
        id (stepper.run/execute-async! ds (db/state-machine ds "sm1") "{}" {:name "e1"})]
    (Thread/sleep 500)
    (let [resp (post handler (str "/execution/" id "/stop") {})]
      (is (= 303 (:status resp)))
      (is (= (str "/execution/" id) (get-in resp [:headers "Location"]))))
    (loop [n 0]
      (when (and (< n 40) (= "RUNNING" (:status (db/execution ds id))))
        (Thread/sleep 100)
        (recur (inc n))))
    (is (= "ABORTED" (:status (db/execution ds id))))))

(deftest delete-machine-via-web-cascades
  (let [ds (fresh-db)
        handler (route ds)
        _ (db/create-state-machine! ds {:id "sm1" :name "m" :definition definition})
        id (stepper.run/execute-async! ds (db/state-machine ds "sm1") "{}" {:name "e1"})
        _ (db/create-schedule! ds {:id "sched1" :state-machine-id "sm1"
                                   :expression "rate(1 hours)" :input nil
                                   :next-run-at "2099-01-01T00:00:00.000Z"})]
    (loop [n 0]
      (when (and (< n 40) (= "RUNNING" (:status (db/execution ds id))))
        (Thread/sleep 50)
        (recur (inc n))))
    (let [resp (post handler "/machine/m/delete" {})]
      (is (= 303 (:status resp)))
      (is (= "/" (get-in resp [:headers "Location"]))))
    (is (nil? (db/state-machine-by-name ds "m")))
    (is (nil? (db/execution ds id)))
    (is (empty? (db/events ds id)))
    (is (nil? (db/schedule ds "sched1")))))

(deftest create-machine-rejects-bad-input
  (let [ds (fresh-db)
        handler (route ds)]
    (is (= 400 (:status (post handler "/machine" {"name" "bad name!" "definition" definition}))))
    (is (= 400 (:status (post handler "/machine" {"name" "x" "definition" "{"}))))
    (post handler "/machine" {"name" "dup" "definition" definition})
    (is (= 400 (:status (post handler "/machine" {"name" "dup" "definition" definition}))))
    (is (nil? (db/state-machine-by-name ds "x")))))
