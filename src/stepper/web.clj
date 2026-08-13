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

(def ^:private style
  "Pico's classless stylesheet follows the reader's light or dark
  preference on its own.  The rest is Stepper's own vocabulary: buttons
  and form controls sit side by side rather than stacking, which is what
  Pico does with bare form elements.  Its classless build ships no colour
  palette either, so the few colours Stepper needs are named here."
  ":root {--green: #2e7d32; --red: #c62828; --amber: #b26a00}
   @media (prefers-color-scheme: dark) {
     :root {--green: #4caf50; --red: #ef5350; --amber: #ffb300}}

   body > header, body > main {max-width: 76rem; margin-inline: auto;
                               padding: 0.25rem 1.5rem 2rem}
   h1 {font-size: 1.9rem; margin: 1rem 0 0}
   h1 a {text-decoration: none}
   h2 {font-size: 1.5rem; margin: 1.75rem 0 0.6rem}
   h3 {font-size: 1.2rem; margin: 1.5rem 0 0.6rem; color: var(--pico-muted-color)}
   p {margin: 0.5rem 0}

   form {margin: 0}
   input, select, textarea {margin: 0}
   button, [type=submit], [type=button] {
     width: auto; display: inline-block; margin: 0;
     padding: 0.5rem 1.1rem}
   label {display: inline-flex; gap: 0.4rem; align-items: center; margin: 0}

   /* Pico stretches bare form controls to full width with a selector of
      its own high specificity, so overriding it takes !important */
   /* stretch, not center: a select is taller than an input, so centering
      leaves their edges ragged */
   .row {display: flex; gap: 0.5rem; align-items: stretch; flex-wrap: wrap;
         margin-block: 0.75rem}
   .row > * {align-self: stretch}
   .row > input, .row > select {width: auto !important; flex: 1 1 14rem}
   .fields {display: flex; flex-direction: column; gap: 0.6rem;
            align-items: start; margin-block: 0.75rem}
   .fields label {flex-direction: column; align-items: start; gap: 0.3rem;
                  width: 100%; font-size: 0.95rem; color: var(--pico-muted-color)}
   .fields textarea {width: 100% !important;
                     font-family: var(--pico-font-family-monospace)}
   .fields > label > input:not([type=checkbox]) {width: auto !important;
                                                 min-width: 20rem}
   .fields label:has(> [type=checkbox]) {flex-direction: row; align-items: center;
                                         width: auto; color: inherit; font-size: 1rem}
   [type=checkbox] {width: 1.15em !important; height: 1.15em;
                    min-width: 0 !important; margin: 0}

   .danger {background: transparent; border-color: var(--red);
            color: var(--red)}
   .danger:hover {background: var(--red); color: var(--pico-background-color);
                  border-color: var(--red)}

   table {font-size: 0.95rem; margin-block: 0.75rem}
   td, th {white-space: nowrap; padding: 0.4rem 0.8rem}
   td pre, td code {white-space: pre-wrap; margin: 0; font-size: 0.9rem}
   td form {display: inline}
   td button {padding: 0.15rem 0.6rem; font-size: 0.85rem}
   code {font-size: 0.95rem}
   pre {max-height: 32em; overflow: auto; font-size: 0.95rem}

   .status {font-weight: bold}
   .SUCCEEDED {color: var(--green)}
   .FAILED {color: var(--red)}
   .RUNNING {color: var(--amber)}")

(defn- render [status & body]
  {:status status
   :headers {"Content-Type" "text/html"}
   :body (str (h/html
               [:html
                [:head
                 [:meta {:charset "utf-8"}]
                 [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
                 [:meta {:name "color-scheme" :content "light dark"}]
                 [:title "Stepper"]
                 [:script {:src "https://unpkg.com/htmx.org@2.0.4"}]
                 [:link {:rel "stylesheet"
                         :href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.classless.min.css"}]
                 ;; raw, or hiccup escapes the child selectors' > into &gt;
                 [:style (h/raw style)]]
                [:body
                 [:header [:h1 [:a {:href "/"} "Stepper"]]]
                 (into [:main] body)]]))})

(defn- page [& body]
  (apply render 200 body))

(defn- error-page
  "Page listing what is wrong, with the rejected text to fix and resubmit."
  [status heading errors {:keys [action field text]}]
  (render status
          [:h2 heading]
          [:ul (for [e errors] [:li e])]
          (when action
            [:form {:method "post" :action action :class "fields"}
             [:textarea {:name field :rows 20} text]
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
     [:form {:method "post" :action "/schedule" :class "row"}
      [:select {:name "machine"}
       (for [m machines] [:option {:value (:name m)} (:name m)])]
      [:input {:name "expression" :placeholder "rate(30 minutes) or */5 * * * *"}]
      [:input {:name "input" :placeholder "{\"at\": \"{% $input.time %}\"}"}]
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
     [:form {:method "post" :action (str "/machine/" name "/start") :class "row"}
      [:input {:name "input" :placeholder "{\"input\": \"json\"}"}]
      [:input {:name "execution" :placeholder "execution name (optional)"}]
      [:button "Start execution"]]
     [:h3 "Executions"]
     [:table
      [:tr [:th "name"] [:th "version"] [:th "status"] [:th "started"] [:th "stopped"]]
      (for [e (db/executions ds (:id machine))
            :let [v (some->> (:state-machine-version-id e) (db/version ds))]]
        [:tr
         [:td [:a {:href (str "/execution/" (:id e))} (:name e)]]
         [:td (:version v)]
         [:td {:class (str "status " (:status e))} (:status e)]
         [:td (:started-at e)]
         [:td (:stopped-at e)]])]
     (let [versions (db/versions ds (:id machine))
           current (first versions)]
       (list
        [:h3 "Definition — version " (:version current)]
        [:form {:method "post" :action (str "/machine/" name "/definition")
                :class "fields"}
         [:textarea {:name "definition" :rows 20} (pretty (:definition current))]
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
     [:p "status: " [:strong {:class (str "status " (:status e))} (:status e)]]
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
       [:h3 "Edit"]
       ;; the buttons live outside the form so they sit side by side,
       ;; tied back to it by the form attribute
       [:form {:id "edit" :method "post" :action (str "/schedule/" id "/edit")
               :class "fields"}
        [:label "expression"
         [:input {:name "expression" :value (:expression s) :size 30}]]
        [:label "event template — JSONata, $input.time is the firing time"
         [:textarea {:name "input" :rows 8} (pretty (:input s))]]
        [:label [:input (cond-> {:type "checkbox" :name "enabled"}
                          (= 1 (:enabled s)) (assoc :checked "checked"))]
         "enabled"]]
       [:div {:class "row"}
        [:button {:form "edit"} "Save"]
        [:form {:method "post" :action (str "/schedule/" id "/delete")}
         [:button {:class "danger"} "Delete schedule"]]]
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
             template (not-empty (str/trim (get params "input" "")))]
         (if-let [errors (seq (scheduler/errors expression template))]
           (error-page 400 "Schedule was not added" errors {})
           (do (db/create-schedule! ds {:id (str (random-uuid))
                                        :state-machine-id (:id machine)
                                        :expression expression
                                        :input template
                                        :next-run-at (str (scheduler/next-run
                                                           expression (java.time.Instant/now)))})
               {:status 303 :headers {"Location" "/"}})))

       (and (= request-method :post) (re-matches #"/schedule/([^/]+)/edit" uri))
       (let [id (second (re-matches #"/schedule/([^/]+)/edit" uri))
             params (form-params request)
             expression (get params "expression")
             template (not-empty (str/trim (get params "input" "")))
             enabled (contains? params "enabled")]
         (if-let [errors (seq (scheduler/errors expression template))]
           (error-page 400 "Schedule was not saved" errors {})
           (do (db/update-schedule! ds id
                                    {:expression expression
                                     :input template
                                     :enabled enabled
                                     :next-run-at (str (scheduler/next-run
                                                        expression (java.time.Instant/now)))})
               {:status 303 :headers {"Location" (str "/schedule/" id)}})))

       (and (= request-method :post) (re-matches #"/schedule/([^/]+)/delete" uri))
       (do (db/delete-schedule! ds (second (re-matches #"/schedule/([^/]+)/delete" uri)))
           {:status 303 :headers {"Location" "/"}})

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
