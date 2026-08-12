(ns stepper.web
  "HTMX web UI: state machines, executions, event history."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [org.httpkit.server :as server]
            [stepper.api :as api]
            [stepper.db :as db]
            [stepper.run :as run]
            [stepper.scheduler :as scheduler]))

(defn- page [& body]
  {:status 200
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

(defn- pretty [json-str]
  (when json-str
    (json/generate-string (json/parse-string json-str) {:pretty true})))

(defn- start-execution!
  "Kick off an execution in the background; returns its id."
  [ds machine input-json]
  (run/execute-async! ds machine input-json
                      {:name (str "web-" (System/currentTimeMillis))}))

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
           [:td (if (= 1 (:enabled s)) "yes" "no")]
           [:td (count (db/firings ds (:id s)))]])]))))

(defn- machine-page [ds name]
  (when-let [machine (db/state-machine-by-name ds name)]
    (page
     [:h2 name]
     [:form {:method "post" :action (str "/machine/" name "/start")}
      [:input {:name "input" :placeholder "{\"input\": \"json\"}" :size 60}]
      [:button "Start execution"]]
     [:h3 "Schedules"]
     [:form {:method "post" :action (str "/machine/" name "/schedule")}
      [:input {:name "expression" :placeholder "rate(30 minutes) or */5 * * * *" :size 30}]
      [:input {:name "input" :placeholder "{\"input\": \"json\"}" :size 30}]
      [:button "Add schedule"]]
     (when-let [ss (seq (db/schedules ds (:id machine)))]
       [:table
        [:tr [:th "expression"] [:th "next run"] [:th "enabled"] [:th "firings"]]
        (for [s ss]
          [:tr
           [:td [:a {:href (str "/schedule/" (:id s))} (:expression s)]]
           [:td (:next-run-at s)]
           [:td (if (= 1 (:enabled s)) "yes" "no")]
           [:td (count (db/firings ds (:id s)))]])])
     [:h3 "Executions"]
     [:table
      [:tr [:th "name"] [:th "status"] [:th "started"] [:th "stopped"]]
      (for [e (db/executions ds (:id machine))]
        [:tr
         [:td [:a {:href (str "/execution/" (:id e))} (:name e)]]
         [:td {:class (:status e)} (:status e)]
         [:td (:started-at e)]
         [:td (:stopped-at e)]])]
     [:h3 "Definition"]
     [:pre (pretty (:definition machine))])))

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
       [:p "next run: " (:next-run-at s) " — " (if (= 1 (:enabled s)) "enabled" "disabled")]
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
     (when (:input e) [:div [:h3 "Input"] [:pre (pretty (:input e))]])
     (execution-fragment ds id))))

(defn- form-params [{:keys [body]}]
  (into {}
        (for [pair (str/split (slurp (or body "")) #"&")
              :let [[k v] (str/split pair #"=" 2)]
              :when k]
          [k (java.net.URLDecoder/decode (or v "") "UTF-8")])))

(defn- handler [ds]
  (fn [{:keys [uri request-method] :as request}]
    (or
     (api/handle ds request)
     (cond
       (= uri "/")
       (index ds)

       (re-matches #"/machine/([^/]+)" uri)
       (machine-page ds (second (re-matches #"/machine/([^/]+)" uri)))

       (and (= request-method :post) (re-matches #"/machine/([^/]+)/schedule" uri))
       (let [name (second (re-matches #"/machine/([^/]+)/schedule" uri))
             machine (db/state-machine-by-name ds name)
             params (form-params request)
             expression (get params "expression")
             next (scheduler/next-run expression (java.time.Instant/now))]
         (db/create-schedule! ds {:id (str (random-uuid))
                                  :state-machine-id (:id machine)
                                  :expression expression
                                  :input (not-empty (get params "input"))
                                  :next-run-at (str next)})
         {:status 303 :headers {"Location" (str "/machine/" name)}})

       (re-matches #"/schedule/([^/]+)" uri)
       (schedule-page ds (second (re-matches #"/schedule/([^/]+)" uri)))

       (and (= request-method :post) (re-matches #"/machine/([^/]+)/start" uri))
       (let [name (second (re-matches #"/machine/([^/]+)/start" uri))
             machine (db/state-machine-by-name ds name)
             execution-id (start-execution! ds machine (get (form-params request) "input"))]
         {:status 303 :headers {"Location" (str "/execution/" execution-id)}})

       (re-matches #"/execution/([^/]+)/fragment" uri)
       {:status 200
        :headers {"Content-Type" "text/html"}
        :body (str (h/html (execution-fragment ds (second (re-matches #"/execution/([^/]+)/fragment" uri)))))}

       (re-matches #"/execution/([^/]+)" uri)
       (execution-page ds (second (re-matches #"/execution/([^/]+)" uri))))
     {:status 404 :body "not found"})))

(defn serve [ds port]
  (server/run-server (handler ds) {:port port})
  (println (str "listening on http://localhost:" port))
  @(promise))
