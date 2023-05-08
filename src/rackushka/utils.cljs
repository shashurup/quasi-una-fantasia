(ns rackushka.utils
  (:require [clojure.set :as set]
            [goog.dom.classes :as gcls]))

(defn parent-elements [subj]
  (iterate #(.-parentElement %) subj))

(defn find-parent-tag [subj tag]
  (first (filter #(= (.-tagName %) tag) (parent-elements subj))))

(defn cycle-style [el style-map]
  (let [class (or (first (set/intersection (set (keys style-map))
                                           (set (gcls/get el)))) "")]
    (gcls/addRemove el class (style-map class))))
