(ns stepper.web
  "HTMX web UI: state machines, executions, event history."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [org.httpkit.server :as server]
            [stepper.api :as api]
            [stepper.db :as db]
            [stepper.run :as run]
            [stepper.scheduler :as scheduler]
            [stepper.validate :as validate]))

(defn- render [status & body]
  {:status status
   :headers {"Content-Type" "text/html"}
   :body (str (h/html
               [:html
                [:head
                 [:meta {:charset "utf-8"}]
                 [:title "Stepper"]
                 [:script {:src "https://unpkg.com/htmx.org@2.0.4"}]
                 [:style "body {font-family: monospace; margin: 2em; max-width: 70em}
                          table {border-collapse: collapse}
                          td, th {border: 1px solid #ccc; padding: 0.3em 0.7em; text-align: left}
                          pre {background: #f5f5f5; padding: 0.7em; overflow-x: auto}
                          .SUCCEEDED {color: green} .FAILED {color: red} .RUNNING {color: orange}"]]
                (into [:body [:h1 [:a {:href "/"} "Stepper"]]] body)]))})

(defn- page [& body]
  (apply render 200 body))

(defn- error-page
  "Page listing what is wrong, with the rejected text to fix and resubmit."
  [status heading errors {:keys [action field text]}]
  (render status
          [:h2 heading]
          [:ul (for [e errors] [:li e])]
          (when action
            [:form {:method "post" :action action}
             [:textarea {:name field :rows 20 :cols 80} text]
             [:br]
             [:button "Try again"]])))

(defn- pretty
  "Pretty-printed JSON; text that does not parse is shown as it is."
  [json-str]
  (when json-str
    (try (json/generate-string (json/parse-string json-str) {:pretty true})
         (catch Exception _ json-str))))

(defn- start-execution!
  "Kick off an execution in the background; returns its id."
  [ds machine input-json execution-name]
  (run/execute-async! ds machine input-json
                      {:name (or (not-empty execution-name)
                                 (run/generated-name "web"))}))

(defn- toggle-button [s back]
  [:form {:method "post" :action (str "/schedule/" (:id s) "/toggle")
          :style "display:inline"}
   [:input {:type "hidden" :name "back" :value back}]
   [:button (if (= 1 (:enabled s)) "disable" "enable")]])

(defn- index [ds]
  (let [machines (db/state-machines ds)
        machine-name (into {} (map (juxt :id :name)) machines)]
    (page
     [:h2 "State machines"]
     [:table
      [:tr [:th "name"] [:th "created"]]
      (for [m machines]
        [:tr
         [:td [:a {:href (str "/machine/" (:name m))} (:name m)]]
         [:td (:created-at m)]])]
     [:h2 "Schedules"]
     [:form {:method "post" :action "/schedule"}
      [:select {:name "machine"}
       (for [m machines] [:option {:value (:name m)} (:name m)])]
      [:input {:name "expression" :placeholder "rate(30 minutes) or */5 * * * *" :size 30}]
      [:input {:name "input" :placeholder "{\"input\": \"json\"}" :size 30}]
      [:button "Add schedule"]]
     (when-let [ss (seq (db/schedules ds))]
       [:table
        [:tr [:th "machine"] [:th "expression"] [:th "next run"] [:th "enabled"] [:th "firings"]]
        (for [s ss]
          [:tr
           [:td [:a {:href (str "/machine/" (machine-name (:state-machine-id s)))}
                 (machine-name (:state-machine-id s))]]
           [:td [:a {:href (str "/schedule/" (:id s))} (:expression s)]]
           [:td (:next-run-at s)]
           [:td (if (= 1 (:enabled s)) "yes" "no") " " (toggle-button s "/")]
           [:td (count (db/firings ds (:id s)))]])]))))

(defn- machine-page [ds name]
  (when-let [machine (db/state-machine-by-name ds name)]
    (page
     [:h2 name]
     [:p "ARN: " [:code (api/machine-arn name)]]
     [:form {:method "post" :action (str "/machine/" name "/start")}
      [:input {:name "input" :placeholder "{\"input\": \"json\"}" :size 45}]
      [:input {:name "execution" :placeholder "execution name (optional)" :size 25}]
      [:button "Start execution"]]
     [:h3 "Executions"]
     [:table
      [:tr [:th "name"] [:th "version"] [:th "status"] [:th "started"] [:th "stopped"]]
      (for [e (db/executions ds (:id machine))
            :let [v (some->> (:state-machine-version-id e) (db/version ds))]]
        [:tr
         [:td [:a {:href (str "/execution/" (:id e))} (:name e)]]
         [:td (:version v)]
         [:td {:class (:status e)} (:status e)]
         [:td (:started-at e)]
         [:td (:stopped-at e)]])]
     (let [versions (db/versions ds (:id machine))
           current (first versions)]
       (list
        [:h3 "Definition — version " (:version current)]
        [:form {:method "post" :action (str "/machine/" name "/definition")}
         [:textarea {:name "definition" :rows 20 :cols 80} (pretty (:definition current))]
         [:br]
         [:button "Save as new version"]]
        [:h3 "Versions"]
        [:table
         [:tr [:th "version"] [:th "created"]]
         (for [v versions]
           [:tr
            [:td [:a {:href (str "/machine/" name "/version/" (:version v))} (:version v)]]
            [:td (:created-at v)]])])))))

(defn- version-page [ds name version]
  (when-let [machine (db/state-machine-by-name ds name)]
    (when-let [v (first (filter #(= version (:version %)) (db/versions ds (:id machine))))]
      (page
       [:h2 [:a {:href (str "/machine/" name)} name] " — version " version]
       [:p "ARN: " [:code (api/version-arn name version)]]
       [:p "created: " (:created-at v)]
       [:pre (pretty (:definition v))]))))

(defn- execution-fragment [ds id]
  (let [e (db/execution ds id)]
    [:div (cond-> {:id "execution"}
            (= "RUNNING" (:status e))
            (assoc :hx-get (str "/execution/" id "/fragment")
                   :hx-trigger "every 1s"
                   :hx-swap "outerHTML"))
     [:p "status: " [:strong {:class (:status e)} (:status e)]]
     (when (:error e) [:p "error: " (:error e) " — " (:cause e)])
     (when (:output e) [:div [:h3 "Output"] [:pre (pretty (:output e))]])
     [:h3 "Events"]
     [:table
      [:tr [:th "time"] [:th "type"] [:th "state"] [:th "detail"]]
      (for [ev (db/events ds id)]
        [:tr
         [:td (:created-at ev)]
         [:td (:type ev)]
         [:td (:state-name ev)]
         [:td [:pre (pretty (:detail ev))]]])]]))

(defn- execution-link
  "Resolve an execution SRN into a link, plain text when not found."
  [ds srn]
  (let [{:keys [machine-name execution-name]} (run/parse-execution-srn srn)
        machine (db/state-machine-by-name ds machine-name)
        e (when machine (db/execution-by-name ds (:id machine) execution-name))]
    (if e
      [:a {:href (str "/execution/" (:id e))} srn]
      srn)))

(defn- schedule-page [ds id]
  (when-let [s (db/schedule ds id)]
    (let [machine (db/state-machine ds (:state-machine-id s))]
      (page
       [:h2 "Schedule " (:expression s)]
       [:p "machine: " [:a {:href (str "/machine/" (:name machine))} (:name machine)]]
       [:p "next run: " (:next-run-at s) " — " (if (= 1 (:enabled s)) "enabled" "disabled")
        " " (toggle-button s (str "/schedule/" id))]
       (when (:input s) [:div [:h3 "Input"] [:pre (pretty (:input s))]])
       [:h3 "Firings"]
       [:table
        [:tr [:th "fired at"] [:th "execution"]]
        (for [f (db/firings ds id)]
          [:tr
           [:td (:fired-at f)]
           [:td (execution-link ds (:execution-srn f))]])]))))

(defn- execution-page [ds id]
  (when-let [e (db/execution ds id)]
    (page
     [:h2 "Execution " (:name e)]
     (let [machine (db/state-machine ds (:state-machine-id e))
           v (some->> (:state-machine-version-id e) (db/version ds))]
       [:p "machine: " [:a {:href (str "/machine/" (:name machine))} (:name machine)]
        (when v
          (list " — version "
                [:a {:href (str "/machine/" (:name machine) "/version/" (:version v))}
                 (:version v)]))])
     (when (:input e) [:div [:h3 "Input"] [:pre (pretty (:input e))]])
     (execution-fragment ds id))))

(defn- form-params [{:keys [body]}]
  (into {}
        (for [pair (str/split (slurp (or body "")) #"&")
              :let [[k v] (str/split pair #"=" 2)]
              :when k]
          [k (java.net.URLDecoder/decode (or v "") "UTF-8")])))

(defn- route [ds]
  (fn [{:keys [uri request-method] :as request}]
    (or
     (api/handle ds request)
     (cond
       (= uri "/")
       (index ds)

       (re-matches #"/machine/([^/]+)" uri)
       (machine-page ds (second (re-matches #"/machine/([^/]+)" uri)))

       (and (= request-method :post) (re-matches #"/machine/([^/]+)/definition" uri))
       (let [name (second (re-matches #"/machine/([^/]+)/definition" uri))
             machine (db/state-machine-by-name ds name)
             definition (get (form-params request) "definition")]
         (if-let [errors (seq (validate/errors definition))]
           (error-page 400 (str "Definition of " name " was not saved") errors
                       {:action (str "/machine/" name "/definition")
                        :field "definition"
                        :text definition})
           (do (db/add-version! ds (:id machine) definition)
               {:status 303 :headers {"Location" (str "/machine/" name)}})))

       (re-matches #"/machine/([^/]+)/version/(\d+)" uri)
       (let [[_ name version] (re-matches #"/machine/([^/]+)/version/(\d+)" uri)]
         (version-page ds name (parse-long version)))

       (and (= request-method :post) (= uri "/schedule"))
       (let [params (form-params request)
             machine (db/state-machine-by-name ds (get params "machine"))
             expression (get params "expression")
             next (try (scheduler/next-run expression (java.time.Instant/now))
                       (catch Exception e
                         {:error (ex-message e)}))]
         (if (:error next)
           (error-page 400 "Schedule was not added"
                       [(str (pr-str expression)
                             " is not a cron expression or rate(N seconds|minutes|hours|days)"
                             " — " (:error next))]
                       {})
           (do (db/create-schedule! ds {:id (str (random-uuid))
                                        :state-machine-id (:id machine)
                                        :expression expression
                                        :input (not-empty (get params "input"))
                                        :next-run-at (str next)})
               {:status 303 :headers {"Location" "/"}})))

       (and (= request-method :post) (re-matches #"/schedule/([^/]+)/toggle" uri))
       (let [id (second (re-matches #"/schedule/([^/]+)/toggle" uri))
             s (db/schedule ds id)
             enable (not= 1 (:enabled s))]
         ;; recompute next run on enable, so a stale schedule does not
         ;; fire immediately for missed time
         (when enable
           (db/set-next-run! ds id (str (scheduler/next-run (:expression s)
                                                            (java.time.Instant/now)))))
         (db/set-enabled! ds id enable)
         {:status 303 :headers {"Location" (get (form-params request) "back" "/")}})

       (re-matches #"/schedule/([^/]+)" uri)
       (schedule-page ds (second (re-matches #"/schedule/([^/]+)" uri)))

       (and (= request-method :post) (re-matches #"/machine/([^/]+)/start" uri))
       (let [name (second (re-matches #"/machine/([^/]+)/start" uri))
             machine (db/state-machine-by-name ds name)
             params (form-params request)]
         (try
           (let [execution-id (start-execution! ds machine
                                                (get params "input")
                                                (get params "execution"))]
             {:status 303 :headers {"Location" (str "/execution/" execution-id)}})
           (catch clojure.lang.ExceptionInfo e
             (error-page 400 (str "Execution of " name " was not started")
                         (:errors (ex-data e))
                         {}))))

       (re-matches #"/execution/([^/]+)/fragment" uri)
       {:status 200
        :headers {"Content-Type" "text/html"}
        :body (str (h/html (execution-fragment ds (second (re-matches #"/execution/([^/]+)/fragment" uri)))))}

       (re-matches #"/execution/([^/]+)" uri)
       (execution-page ds (second (re-matches #"/execution/([^/]+)" uri))))
     (render 404 [:h2 "Not found"] [:p uri]))))

(defn- handler
  "Routes a request, turning a failure into a page that says what broke."
  [ds]
  (let [route (route ds)]
    (fn [request]
      (try (route request)
           (catch Exception e
             (error-page 500 "Something went wrong"
                         (or (:errors (ex-data e)) [(ex-message e)])
                         {}))))))

(defn serve [ds port]
  (server/run-server (handler ds) {:port port})
  (println (str "listening on http://localhost:" port))
  @(promise))
