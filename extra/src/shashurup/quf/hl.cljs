(ns shashurup.quf.hl
  (:require [clojure.string :as s]
            [shashurup.quf.render :refer [render hint-args]]
            [crate.core :as crate]
            [hljs]))

(defn highlight-auto [subj langs]
  (let [subset (when langs
                 (if (coll? langs)
                   (to-array langs)
                   (array langs)))
        result (hljs/highlightAuto subj subset)]
    {:value (.-value result)
     :language (.-language result)
     :relevance (.-relevance result)}))

(defn- from-raw-html [subj]
  (let [templ (.createElement js/document "template")]
    (set! (.-innerHTML templ) subj)
    (.-content templ)))

(defmethod render :code [subj]
  (let [[_ langs] (hint-args subj)
        code (s/join "\n" subj)
        {hled :value} (highlight-auto code langs)]
    [:pre [:code (from-raw-html hled)]]))
