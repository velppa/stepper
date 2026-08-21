(ns stepper.auth-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [org.httpkit.server :as server]
            [stepper.auth :as auth]))

(defn- with-provider
  "Run F with a stub home-auth listening on a free port; F gets the
  base URL and an atom holding the names that were asked for."
  [handler f]
  (let [asked (atom [])
        stop (server/run-server
              (fn [{:keys [uri query-string]}]
                (swap! asked conj query-string)
                (handler uri query-string))
              {:port 0 :legacy-return-value? false})
        port (server/server-port stop)]
    (try
      (f (str "http://127.0.0.1:" port) asked)
      (finally (server/server-stop! stop)))))

(deftest fetch-local-credentials-reads-the-provider-reply
  (with-provider
    (fn [uri _]
      (if (= uri "/local/client")
        {:status 200
         :body (json/generate-string {"issuer" "https://auth.example/oidc/"
                                      "client_id" "stepper"
                                      "client_secret" "s3cret"})}
        {:status 404 :body "not found"}))
    (fn [base asked]
      (let [cfg (auth/fetch-local-credentials base "Stepper")]
        (is (= "https://auth.example/oidc" (:issuer cfg)) "trailing slash trimmed")
        (is (= "stepper" (:client-id cfg)))
        (is (= "s3cret" (:client-secret cfg)))
        (is (= ["name=Stepper"] @asked))))))

(deftest fetch-local-credentials-throws-on-refusal
  (with-provider
    (fn [_ _] {:status 404 :body "no client named Nope"})
    (fn [base _]
      (is (thrown? clojure.lang.ExceptionInfo
                   (auth/fetch-local-credentials base "Nope"))))))

(deftest fetch-local-credentials-maps-tokens-to-users
  (with-provider
    (fn [_ _]
      {:status 200
       :body (json/generate-string
              {"issuer" "https://auth.example/oidc"
               "client_id" "stepper" "client_secret" "s3cret"
               "tokens" [{"name" "api" "token" "t0ken" "username" "velppa"}]})})
    (fn [base _]
      (is (= {"t0ken" "velppa"}
             (:api-tokens (auth/fetch-local-credentials base "Stepper")))))))

(deftest bearer-user-accepts-only-known-tokens
  (let [cfg {:api-tokens {"t0ken" "velppa"}}
        req #(hash-map :headers {"authorization" %})]
    (is (= "velppa" (auth/bearer-user cfg (req "Bearer t0ken"))))
    (is (nil? (auth/bearer-user cfg (req "Bearer nope"))))
    (is (nil? (auth/bearer-user cfg (req "t0ken"))) "scheme required")
    (is (nil? (auth/bearer-user cfg {:headers {}})))
    (is (= "" (auth/bearer-user {:api-tokens {"envtok" nil}} (req "Bearer envtok")))
        "a token from the environment has no user")))

(deftest machine-endpoints-need-a-known-token
  (let [cfg {:base-url "https://stepper.example"
             :issuer "https://auth.example/oidc"
             :client-id "stepper" :client-secret "s3cret"
             :api-tokens {"t0ken" "velppa"}}
        handler (auth/wrap cfg (fn [_] {:status 200 :body "ok"}))
        poll #(handler {:uri "/client/poll" :headers %})]
    (is (= 200 (:status (poll {"authorization" "Bearer t0ken"}))))
    (is (= 401 (:status (poll {"authorization" "Bearer nope"}))))
    (is (= 401 (:status (poll {}))))))
