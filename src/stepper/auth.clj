(ns stepper.auth
  "SSO for the web UI via an OIDC provider (home-auth), plus a shared
  bearer token for the machine endpoints.

  Enabled by STEPPER_BASE_URL, the public origin the provider redirects
  back to; without it every request passes, which is the local
  single-user mode.  Browser requests need a session established by /auth/login ->
  provider -> /auth/callback; the client long-poll/result endpoints and
  the AWS-protocol API need Authorization: Bearer with one of the API
  tokens home-auth holds for Stepper."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.client :as http]))

(defn fetch-local-credentials
  "Ask home-auth on this host for the credentials registered under
  NAME, so the secret never has to live in the environment.  A refused
  connection means the provider has not come up yet and is retried;
  anything it answers is a verdict.  Returns a map with :issuer,
  :client-id, :client-secret and :api-tokens (token -> username), or
  throws."
  [local-url name]
  (let [endpoint (str (str/replace local-url #"/+$" "") "/local/client?name="
                      (java.net.URLEncoder/encode name "UTF-8"))]
    (loop [attempt 0]
      (let [{:keys [status body error]} @(http/get endpoint {:as :text})
            reply (when (= 200 status) (json/parse-string body))]
        (cond
          (and reply (get reply "client_id") (get reply "client_secret"))
          {:issuer (str/replace (or (get reply "issuer") "") #"/+$" "")
           :client-id (get reply "client_id")
           :client-secret (get reply "client_secret")
           :api-tokens (into {} (for [t (get reply "tokens")
                                      :let [token (get t "token")]
                                      :when (seq token)]
                                  [token (get t "username")]))}

          (and error (< attempt 4))
          (do (Thread/sleep 2000) (recur (inc attempt)))

          :else
          (throw (ex-info (str "asking " local-url " for " (pr-str name)
                               " credentials failed")
                          {:status status :body body :error error})))))))

(def ^:private default-client-name
  "Name Stepper is registered under in home-auth."
  "Stepper")

(defn config
  "Auth configuration; nil = auth disabled.  Credentials and API tokens
  come from home-auth over loopback, looked up by client name;
  STEPPER_OIDC_ISSUER, STEPPER_OIDC_CLIENT_ID and
  STEPPER_OIDC_CLIENT_SECRET override the pieces they name, and giving
  all three skips the lookup.  STEPPER_API_TOKEN adds a token to the
  accepted set.  The name itself comes from STEPPER_OIDC_CLIENT_NAME,
  the provider address from STEPPER_OIDC_LOCAL_URL."
  []
  (let [base-url (str/replace (or (System/getenv "STEPPER_BASE_URL") "") #"/+$" "")
        env-issuer (System/getenv "STEPPER_OIDC_ISSUER")
        env-id (System/getenv "STEPPER_OIDC_CLIENT_ID")
        env-secret (System/getenv "STEPPER_OIDC_CLIENT_SECRET")
        env-token (System/getenv "STEPPER_API_TOKEN")]
    (when (or (seq base-url) env-issuer)
      (let [fetched (when-not (and env-issuer env-id env-secret)
                      (fetch-local-credentials
                       (or (System/getenv "STEPPER_OIDC_LOCAL_URL") "http://127.0.0.1:3001")
                       (or (System/getenv "STEPPER_OIDC_CLIENT_NAME") default-client-name)))]
        (merge {:base-url base-url}
               fetched
               (cond-> {}
                 env-issuer (assoc :issuer (str/replace env-issuer #"/+$" ""))
                 env-id (assoc :client-id env-id)
                 env-secret (assoc :client-secret env-secret))
               {:api-tokens (cond-> (:api-tokens fetched)
                              env-token (assoc env-token nil))})))))

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

(defn bearer-user
  "Username the request's bearer token acts as, or nil when the token
  is not one of the app's.  A token configured through the environment
  has no user, so it yields \"\" rather than nil."
  [{:keys [api-tokens]} request]
  (when-let [header (get-in request [:headers "authorization"])]
    (let [token (second (re-find #"^Bearer\s+(.+)$" header))]
      (when (and token (contains? api-tokens token))
        (or (get api-tokens token) "")))))

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
        (if (bearer-user cfg request)
          (handler request)
          {:status 401 :body "unauthorized"})
        :else
        (if (cookie-session request)
          (handler request)
          {:status 302 :headers {"Location" "/auth/login"}})))))
