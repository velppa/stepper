(ns stepper.web-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [stepper.db :as db]
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

(deftest create-machine-rejects-bad-input
  (let [ds (fresh-db)
        handler (route ds)]
    (is (= 400 (:status (post handler "/machine" {"name" "bad name!" "definition" definition}))))
    (is (= 400 (:status (post handler "/machine" {"name" "x" "definition" "{"}))))
    (post handler "/machine" {"name" "dup" "definition" definition})
    (is (= 400 (:status (post handler "/machine" {"name" "dup" "definition" definition}))))
    (is (nil? (db/state-machine-by-name ds "x")))))
