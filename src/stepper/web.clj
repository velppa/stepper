(ns stepper.web
  "HTMX web UI: state machines, executions, event history."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [org.httpkit.server :as server]
            [stepper.api :as api]
            [stepper.db :as db]
            [stepper.run :as run]))

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
  (page
   [:h2 "State machines"]
   [:table
    [:tr [:th "name"] [:th "created"]]
    (for [m (db/state-machines ds)]
      [:tr
       [:td [:a {:href (str "/machine/" (:name m))} (:name m)]]
       [:td (:created-at m)]])]))

(defn- machine-page [ds name]
  (when-let [machine (db/state-machine-by-name ds name)]
    (page
     [:h2 name]
     [:form {:method "post" :action (str "/machine/" name "/start")}
      [:input {:name "input" :placeholder "{\"input\": \"json\"}" :size 60}]
      [:button "Start execution"]]
     (when-let [ss (seq (db/schedules ds (:id machine)))]
       (list
        [:h3 "Schedules"]
        [:table
         [:tr [:th "expression"] [:th "next run"] [:th "enabled"]]
         (for [s ss]
           [:tr
            [:td (:expression s)]
            [:td (:next-run-at s)]
            [:td (if (= 1 (:enabled s)) "yes" "no")]])]))
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
