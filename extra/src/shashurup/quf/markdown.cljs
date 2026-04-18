(ns shashurup.quf.markdown
  (:require [clojure.string :as s]
            [shashurup.quf.render :refer [render hint-args]]
            [marked]
            [hljs]))

(defn parse [subj]
  (.parse js/marked subj))

(defn render-code [subj]
  (str "<pre><code>"
       (.-value (hljs/highlightAuto (.-text subj)
                                    (to-array [(.-lang subj)])))
       "</code></pre>"))

(defn render-table [subj]
  (let [rndr #(.parseInline js/marked (.-text %))]
    (str "<table class=quf >"
         "<thead><tr>"
         (apply str (for [h (.-header subj)]
                      (str "<th class=quf>" (rndr h) "</th>")))
         "</tr></thead>"
         "<tbody>"
         (apply str (for [row (.-rows subj)]
                      (apply str
                             "<tr>"
                             (apply str (for [cell row]
                                          (str "<td class=quf>"
                                               (rndr cell)
                                               "</td>")))
                             "</tr>"
                             )))
         "</tbody>"
         "</table>")))

(.use js/marked (clj->js {:renderer {:code render-code
                                     :table render-table}}))

(defn- from-raw-html [subj]
  (let [templ (.createElement js/document "template")]
    (set! (.-innerHTML templ) subj)
    (.-content templ)))

(defmethod render :markdown [subj]
  [:div (from-raw-html (parse (s/join "\n" subj)))])
