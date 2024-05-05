(ns shashurup.quf.ui
  (:require [shashurup.quf.response :as r]
            [clojure.string :as s]))

(defn load-cells [source]
  (r/hint (if (symbol? source)
            source
            [(slurp source)]) :cells))

(defn store-cells [path cells]
  (spit path (s/join "\n\n" cells)))

(defn table [subj] (r/hint subj :table))

(defn html [subj] (r/hint subj :html))

(defn keymap [subj] (r/hint subj :keymap))

(defn raw [subj] (vary-meta subj dissoc :shashurup.quf/hint))
