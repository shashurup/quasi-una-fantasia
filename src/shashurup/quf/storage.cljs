(ns shashurup.quf.storage
  (:require [clojure.string :as s]
            [cljs.tools.reader :refer [read-string]]))

(defn stored-nses []
  (->> (range)
       (map #(.key (.-localStorage js/window) %))
       (take-while identity)
       (filter #(s/starts-with? % "ns."))
       (mapv #(subs % 3))))

(defn load-ns-exprs [ns]
  (-> (.-localStorage js/window)
      (.getItem (str "ns." ns))
      read-string
      (or [])))

(defn store-ns-exprs [ns exprs]
  (.setItem (.-localStorage js/window)
            (str "ns." name)
            (pr-str exprs)))
