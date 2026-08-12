(ns stepper.validate
  "Validation of state machine definitions.

  Checks the ASL structure Stepper supports and compiles every JSONata
  expression, so a definition is rejected before it is stored rather
  than failing mid-execution."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import (com.dashjoin.jsonata Jsonata)))

(def ^:private state-types
  #{"Pass" "Task" "Choice" "Wait" "Succeed" "Fail" "Parallel" "Map"})

(def ^:private terminal-types #{"Succeed" "Fail"})

(defn- expression? [x]
  (and (string? x) (str/starts-with? x "{%") (str/ends-with? x "%}")))

(defn- expression-errors [where x]
  (cond
    (expression? x) (try (Jsonata/jsonata (subs x 2 (- (count x) 2))) nil
                         (catch Exception e
                           [(str where ": invalid JSONata expression " (pr-str x)
                                 " — " (ex-message e))]))
    (map? x) (mapcat #(expression-errors where %) (vals x))
    (sequential? x) (mapcat #(expression-errors where %) x)))

(declare machine-errors)

(defn- state-errors [where name state]
  (let [where (str where "state " (pr-str name))
        type (get state "Type")]
    (concat
     (when-not (map? state) [(str where " must be an object")])
     (cond
       (nil? type) [(str where " has no Type")]
       (not (state-types type)) [(str where " has unsupported Type " (pr-str type))])
     (when (and (state-types type)
                (not (terminal-types type))
                (not= "Choice" type)
                (not (get state "Next"))
                (not (get state "End")))
       [(str where " needs either Next or \"End\": true")])
     (case type
       "Task" (when-not (get state "Resource")
                [(str where " has no Resource")])
       "Choice" (let [choices (get state "Choices")]
                  (concat
                   (when-not (seq choices) [(str where " has no Choices")])
                   (for [c choices
                         :when (not (and (get c "Condition") (get c "Next")))]
                     (str where " has a choice without Condition and Next"))))
       "Wait" (when-not (or (get state "Seconds") (get state "Timestamp"))
                [(str where " needs Seconds or Timestamp")])
       "Parallel" (let [branches (get state "Branches")]
                    (if-not (seq branches)
                      [(str where " has no Branches")]
                      (mapcat #(machine-errors (str where ", branch: ") %) branches)))
       "Map" (if-let [processor (get state "ItemProcessor")]
               (machine-errors (str where ", item processor: ") processor)
               [(str where " has no ItemProcessor")])
       nil)
     (expression-errors where state))))

(defn- machine-errors
  "Errors of a definition body — the top level or a Parallel branch or
  Map processor, which have the same shape."
  [where definition]
  (let [{start "StartAt" states "States" ql "QueryLanguage"} definition
        names (set (keys states))]
    (concat
     (when (and ql (not= ql "JSONata"))
       [(str where "QueryLanguage " (pr-str ql) " is not supported, only \"JSONata\"")])
     (cond
       (not (map? states)) [(str where "States must be an object")]
       (empty? states) [(str where "States is empty")])
     (cond
       (nil? start) [(str where "StartAt is missing")]
       (and (seq names) (not (names start)))
       [(str where "StartAt " (pr-str start) " is not one of the States")])
     ;; every Next, Default and Catch target must exist
     (for [[name state] states
           :let [targets (concat [(get state "Next") (get state "Default")]
                                 (map #(get % "Next") (get state "Choices"))
                                 (map #(get % "Next") (get state "Catch")))]
           target targets
           :when (and target (not (names target)))]
       (str where "state " (pr-str name) " points at unknown state " (pr-str target)))
     (mapcat (fn [[name state]] (state-errors where name state)) states))))

(defn execution-name-errors
  "Errors of a user-chosen execution name, empty when it is usable."
  [name]
  (concat
   (when (str/blank? name) ["execution name is empty"])
   (when (> (count name) 80) ["execution name is longer than 80 characters"])
   (when (re-find #"[\s<>{}\[\]?*\"#%\\^|~`$&,;:/]" name)
     [(str "execution name " (pr-str name)
           " contains whitespace or one of <>{}[]?*\"#%\\^|~`$&,;:/")])))

(defn errors
  "Errors of DEFINITION-JSON as a seq of messages, empty when it is valid."
  [definition-json]
  (let [parsed (try (json/parse-string definition-json)
                    (catch Exception e
                      ;; the parser appends its own source location, which
                      ;; says nothing the message does not
                      {::unparsable (first (str/split-lines (ex-message e)))}))]
    (cond
      (::unparsable parsed) [(str "not valid JSON — " (::unparsable parsed))]
      (not (map? parsed)) ["definition must be a JSON object"]
      :else (remove nil? (machine-errors "" parsed)))))
