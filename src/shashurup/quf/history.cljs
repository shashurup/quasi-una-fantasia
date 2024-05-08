(ns shashurup.quf.history
  (:require [shashurup.quf.utils :as u]
            [clojure.string :as s]
            [cljs.tools.reader :refer [read-string]]))

(def hist-item "history")

(defn read-local []
  (-> (.-localStorage js/window)
      (.getItem hist-item)
      read-string
      (or [])))

(defn includes-all-substrings? [subj substrings]
  (every? #(s/includes? subj %) substrings))

(defn matches [history terms]
  (->> history
       rseq
       (filter #(includes-all-substrings? % terms))
       distinct))

(defn search [terms]
  (matches (or (u/load-item hist-item) []) terms))

(defn log [expr]
  (u/store-item hist-item (conj (u/load-item hist-item) expr)))
