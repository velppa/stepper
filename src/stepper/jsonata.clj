(ns stepper.jsonata
  "JSONata evaluation over Clojure data.

  dashjoin/jsonata-java operates on java.util collections, so values
  cross the boundary as mutable Java structures and come back as
  Clojure data."
  (:import (com.dashjoin.jsonata Jsonata)))

(defn- to-java [x]
  (cond
    (map? x) (let [m (java.util.LinkedHashMap.)]
               (doseq [[k v] x]
                 (.put m (if (keyword? k) (name k) k) (to-java v)))
               m)

    (sequential? x) (java.util.ArrayList. ^java.util.Collection (mapv to-java x))

    (keyword? x) (name x)

    :else x))

(defn- to-clojure [x]
  (cond
    (instance? java.util.Map x)
    (into {} (map (fn [[k v]] [(str k) (to-clojure v)])) x)

    (instance? java.util.List x)
    (mapv to-clojure x)

    :else x))

(defn evaluate
  "Evaluate JSONata EXPR against DATA with BINDINGS as external variables.
  DATA and BINDINGS are Clojure data with string keys; result is the same."
  ([expr data] (evaluate expr data {}))
  ([expr data bindings]
   (let [j (Jsonata/jsonata expr)
         frame (.createFrame j)]
     (doseq [[k v] bindings]
       (.bind frame (if (keyword? k) (name k) k) (to-java v)))
     (to-clojure (.evaluate j (to-java data) frame)))))
