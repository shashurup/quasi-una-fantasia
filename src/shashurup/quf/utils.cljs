(ns shashurup.quf.utils
  (:require [clojure.set :as set]
            [cljs.loader :as loader]
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

(defn add-script [src]
  (gdom/appendChild 
   (first (gdom/getElementsByTagName "head"))
   (crate/html [:script {:src src}])))

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

;; cljs.loader seems to be a bit broken
;; cljs.loader/load is asynchronous but doesn't allow
;; loading more than one module at time
;; and while goog.ModuleManager is capable of queueing
;; cljs.loader prevents this by calling beforeLoadModuleCode()
;; to early
(defn begin-module-load! [subj]
  (let [mname (-> subj name munge)]
    (.beforeLoadModuleCode loader/*module-manager* mname)))

(defn set-module-loaded! []
  (.setLoaded loader/*module-manager*))

(defn load-module [subj]
  (let [mname (-> subj name munge)]
    (.load loader/*module-manager* mname)))

(defn module? [subj]
  (contains? loader/module-infos subj))
