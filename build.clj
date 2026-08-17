(ns build
  "clojure -T:build uber  ->  target/stepper-client.jar"
  (:require [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def uber-file "target/stepper-client.jar")

(defn uber [_]
  (b/delete {:path "target"})
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
    (b/compile-clj {:basis basis :ns-compile '[stepper.client]
                    :class-dir class-dir})
    (b/uber {:class-dir class-dir :uber-file uber-file
             :basis basis :main 'stepper.client})))
