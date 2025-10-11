(ns shashurup.quf.ui
  (:require [shashurup.quf.response :as r]
            [clojure.string :as s]))

(defn load-cells [source]
  (r/hint (if (symbol? source)
            source
            [(slurp source)]) :cells))

(defn store-cells [path cells]
  (spit path (s/join "\n\n" cells)))

(defn table
  ([cols subj] (r/hint subj [:table cols]))
  ([subj] (r/hint subj :table)))

(defn tree
  ([opts subj] (r/hint subj [:tree opts]))
  ([subj] (r/hint subj :tree)))

(defn sequence [subj] (r/hint subj :sequence))

(defn text [subj]
  (if (string? subj)
    (r/hint [subj] :text)
    (r/hint subj :text)))

(defn code [subj & args]
  (let [hint (if (empty? args)
               :code
               [:code args])]
    (if (string? subj)
      (r/hint [subj] hint)
      (r/hint subj hint))))

(defn html [subj] (r/hint subj :html))

(defn keymap [subj] (r/hint subj :keymap))

(defn raw [subj] (vary-meta subj dissoc :shashurup.quf/hint))
