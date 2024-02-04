(ns shashurup.quf.utils
  (:require [clojure.set :as set]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [goog.dom.classes :as gcls]))

(defn parent-elements [subj]
  (iterate #(.-parentElement %) subj))

(defn find-parent-tag [subj tag]
  (first (filter #(= (.-tagName %) tag) (parent-elements subj))))

(defn cycle-style [el style-map]
  (let [class (or (first (set/intersection (set (keys style-map))
                                           (set (gcls/get el)))) "")]
    (gcls/addRemove el class (style-map class))))

(defn add-style-ref [ref]
  (gdom/appendChild 
   (first (gdom/getElementsByTagName "head"))
   (crate/html [:link {:href ref :rel "stylesheet" :type "text/css"}])))

(defn take-until [pred coll]
  (->> coll
       (partition-by pred)
       (partition 2)
       first
       flatten))

(defonce fn-counter (atom 0))

(defonce fns (atom {}))

(defn gen-js-call
  ([f] (gen-js-call f "fn"))
  ([f prefix]
   (let [name (str prefix (swap! fn-counter inc))]
     (swap! fns assoc name f)
     (str "shashurup.quf.utils.call(\"" name "\");"))))

(defn call [name]
  (let [f (@fns name)]
    (f)))
