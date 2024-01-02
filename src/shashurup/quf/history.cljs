(ns shashurup.quf.history
  (:require [clojure.string :as s]
            [cljs.tools.reader :refer [read-string]]))

(defn read-local []
  (-> (.-localStorage js/window)
      (.getItem "history")
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
  (matches (read-local) terms))

(defn log [expr]
  (.setItem (.-localStorage js/window)
            "history"
            (pr-str (conj (read-local) expr))))
