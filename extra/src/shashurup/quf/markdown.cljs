(ns shashurup.quf.markdown
  (:require [clojure.string :as s]
            [shashurup.quf.render :refer [render hint-args]]
            [marked]))

(defn parse [subj]
  (.parse js/marked subj))

(defn- from-raw-html [subj]
  (let [templ (.createElement js/document "template")]
    (set! (.-innerHTML templ) subj)
    (.-content templ)))

(defmethod render :markdown [subj]
  [:div (from-raw-html (parse (s/join "\n" subj)))])
