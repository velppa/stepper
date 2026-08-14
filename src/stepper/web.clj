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
  "Stepper's stylesheet, sharing home-auth's sourcehut flavour: flat,
  square, compact.  Every rule here is the only rule for what it styles."
  "/* sourcehut-flavoured: flat, square, compact */
   :root{color-scheme:light dark;
   --bg:#fff;--fg:#000;--muted:#555;--rule:#444;--hair:#ddd;
   --field:#888;--edge:#222;--link:#00e;--danger:#a00;
   --ok:#070;--warn:#960;
   --mono:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}
   @media (prefers-color-scheme:dark){
   :root{--bg:#121212;--fg:#ddd;--muted:#999;--rule:#666;--hair:#333;
   --field:#555;--edge:#aaa;--link:#7ab7ff;--danger:#f66;
   --ok:#6c6;--warn:#fc6}}

   *,*::before,*::after{box-sizing:border-box}
   body{font-family:sans-serif;font-size:.9rem;color:var(--fg);background:var(--bg);
   max-width:60rem;margin:0 auto;padding:1.5rem 1rem;line-height:1.4}

   h1{font-size:1.3rem;border-bottom:1px solid var(--rule);
   padding-bottom:.25rem;margin:0 0 .5rem}
   h1 a{color:inherit;text-decoration:none}
   h2{font-size:1.05rem;font-weight:bold;margin:1.75rem 0 .4rem}
   h3{font-size:.9rem;font-weight:bold;margin:1.5rem 0 .3rem;color:var(--muted)}
   p{margin:.5rem 0}
   a{color:var(--link);text-decoration:none}
   a:hover{text-decoration:underline}

   code,pre{font-family:var(--mono)}
   code{font-size:.85em;word-break:break-all}
   pre{font-size:.8rem;border:1px solid var(--hair);padding:.6rem;
   margin:.5rem 0;overflow:auto;max-height:32em}
   pre code{font-size:inherit;word-break:normal}

   input,select,textarea,button{font-size:15px;padding:.3rem .5rem;
   border-radius:0;border:1px solid var(--field);box-sizing:border-box;
   background:var(--bg);color:var(--fg);margin:0}
   input::placeholder,textarea::placeholder{color:var(--muted)}
   textarea{width:100%;font-family:var(--mono);font-size:.8rem;resize:vertical}
   select{appearance:none;padding-right:1.6rem;
   background-image:linear-gradient(45deg,transparent 50%,currentColor 50%),
   linear-gradient(135deg,currentColor 50%,transparent 50%);
   background-position:right .95rem center,right .7rem center;
   background-size:.25rem .25rem;background-repeat:no-repeat}
   [type=checkbox]{width:auto;padding:0}
   button{padding:.25rem .9rem;border:1px solid var(--edge);border-radius:0;
   font-weight:600;background:var(--bg);color:var(--edge);cursor:pointer}
   button:hover{background:var(--edge);color:var(--bg)}
   .danger{border-color:var(--danger);color:var(--danger)}
   .danger:hover{background:var(--danger);color:var(--bg)}

   .row{display:flex;gap:.4rem;align-items:stretch;flex-wrap:wrap;margin:.6rem 0}
   .row > input,.row > select{flex:1 1 12rem;min-width:0}
   .fields{display:flex;flex-direction:column;align-items:flex-start;
   gap:.6rem;margin:.6rem 0}
   .fields label{display:flex;flex-direction:column;gap:.15rem;
   width:100%;color:var(--muted);font-size:.85em}
   .fields label:has(> [type=checkbox]){
   flex-direction:row;align-items:center;gap:.35rem;
   width:auto;color:var(--fg);font-size:inherit}
   .fields > label > input:not([type=checkbox]){width:min(26rem,100%)}

   table{width:100%;border-collapse:collapse;margin:.4rem 0 1rem;font-size:.85rem}
   th{text-align:left;font-weight:600;color:var(--muted)}
   th,td{padding:.25rem .6rem .25rem 0;border-bottom:1px solid var(--hair);
   white-space:nowrap}
   td pre,td code{margin:0;padding:0;border:0;white-space:pre-wrap;font-size:.8rem}
   td form{display:inline}
   td button{padding:.05rem .5rem;font-size:.8rem}

   .status{font-weight:600}
   .SUCCEEDED{color:var(--ok)}
   .FAILED{color:var(--danger)}
   .RUNNING{color:var(--warn)}")

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
