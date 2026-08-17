(ns stepper.auth
  "SSO for the web UI via an OIDC provider (home-auth), plus a shared
  bearer token for the machine endpoints.

  Enabled when STEPPER_OIDC_ISSUER is set; without it every request
  passes, which is the local single-user mode.  Browser requests need a
  session established by /auth/login -> provider -> /auth/callback;
  the client long-poll/result endpoints and the AWS-protocol API need
  Authorization: Bearer $STEPPER_API_TOKEN."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.client :as http]))

(defn config
  "Auth configuration from the environment; nil = auth disabled."
  []
  (when-let [issuer (System/getenv "STEPPER_OIDC_ISSUER")]
    {:issuer (str/replace issuer #"/+$" "")
     :client-id (System/getenv "STEPPER_OIDC_CLIENT_ID")
     :client-secret (System/getenv "STEPPER_OIDC_CLIENT_SECRET")
     :base-url (str/replace (or (System/getenv "STEPPER_BASE_URL") "") #"/+$" "")
     :api-token (System/getenv "STEPPER_API_TOKEN")}))

(defonce ^:private sessions (atom {})) ; sid -> {:sub ...}
(defonce ^:private states (atom #{}))  ; outstanding login round-trips

(defn- random-token []
  (let [bytes (byte-array 32)]
    (.nextBytes (java.security.SecureRandom.) bytes)
    (.encodeToString (java.util.Base64/getUrlEncoder) bytes)))

(defn- cookie-session
  "Session id from the request's cookie header, when it is live."
  [request]
  (some->> (get-in request [:headers "cookie"])
           (re-find #"stepper_session=([^;\s]+)")
           second
           (#(when (contains? @sessions %) %))))

(defn- jwt-claims
  "Claims of a JWT without signature verification - the token arrives
  on the back channel straight from the issuer over TLS."
  [jwt]
  (-> (str/split jwt #"\.")
      second
      (#(.decode (java.util.Base64/getUrlDecoder) ^String %))
      String.
      json/parse-string))

(defn- login
  "Send the browser to the provider's authorization endpoint."
  [{:keys [issuer client-id base-url]}]
  (let [state (random-token)]
    (swap! states conj state)
    {:status 302
     :headers {"Location"
               (str issuer "/auth?"
                    "response_type=code"
                    "&client_id=" (java.net.URLEncoder/encode (str client-id) "UTF-8")
                    "&redirect_uri=" (java.net.URLEncoder/encode
                                      (str base-url "/auth/callback") "UTF-8")
                    "&scope=openid"
                    "&state=" state)}}))

(defn- callback
  "Exchange the code for an id_token and open a session."
  [{:keys [issuer client-id client-secret base-url]} request]
  (let [query (or (:query-string request) "")
        param #(some-> (re-find (re-pattern (str % "=([^&]*)")) query) second
                       (java.net.URLDecoder/decode "UTF-8"))
        code (param "code")
        state (param "state")]
    (if-not (and code state (contains? @states state))
      {:status 400 :body "bad login round-trip"}
      (do
        (swap! states disj state)
        (let [{:keys [status body]}
              @(http/post (str issuer "/token")
                          {:form-params {"grant_type" "authorization_code"
                                         "code" code
                                         "redirect_uri" (str base-url "/auth/callback")
                                         "client_id" client-id
                                         "client_secret" client-secret}
                           :as :text})]
          (if (= 200 status)
            (let [sub (get (jwt-claims (get (json/parse-string body) "id_token")) "sub")
                  sid (random-token)]
              (swap! sessions assoc sid {:sub sub})
              {:status 302
               :headers {"Location" "/"
                         "Set-Cookie" (str "stepper_session=" sid
                                           "; Path=/; HttpOnly; SameSite=Lax"
                                           "; Max-Age=" (* 30 24 3600))}})
            {:status 502 :body (str "token exchange failed: " status)}))))))

(defn- machine-request?
  "Requests authenticated by bearer token rather than a browser session:
  client long-poll/result and the AWS-protocol API."
  [{:keys [uri headers]}]
  (or (str/starts-with? uri "/client/")
      (contains? headers "x-amz-target")))

(defn wrap
  "Guard HANDLER with CFG; with a nil CFG requests pass untouched."
  [cfg handler]
  (if-not cfg
    handler
    (fn [{:keys [uri] :as request}]
      (cond
        (= uri "/auth/login") (login cfg)
        (str/starts-with? uri "/auth/callback") (callback cfg request)
        (machine-request? request)
        (if (and (:api-token cfg)
                 (= (get-in request [:headers "authorization"])
                    (str "Bearer " (:api-token cfg))))
          (handler request)
          {:status 401 :body "unauthorized"})
        :else
        (if (cookie-session request)
          (handler request)
          {:status 302 :headers {"Location" "/auth/login"}})))))
