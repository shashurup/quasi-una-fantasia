(ns shashurup.quf.chartjs
  (:require [shashurup.quf.render :refer [render re-render defer]]
            [shashurup.quf.utils :as u]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [chartjs]))

(u/begin-module-load! :chartjs)

(defn convert-dataset [subj type]
  (if (and (coll? subj)
           (not (coll? (first subj)))
           (#{:line :bar} type))
    (if (= type :line)
      (map-indexed #(hash-map :x (str %1) :y %2) subj)
      (for [x subj] {:x (str x) :y x}))
    subj))

(defn chart-data [subj type]
  (cond
    (and (map? subj)
         (:datasets subj))
    subj

    (map? subj)
    {:datasets (for [[k v] subj]
                 {:label (name k) :data (convert-dataset v type)})}

    :else
    {:datasets [{:data (convert-dataset subj type)}]}))

(defn chart-input [data type]
  (let [data (chart-data data type)
        display-legend (->> (:datasets data)
                            (some :label))]
    (.log js/console display-legend)
    {:type (name type)
     :data data
     :options (condp = type
                :bar {:plugins {:legend {:display display-legend}}}
                :line {:plugins {:legend {:display display-legend}}
                       :animation false}
                :scatter {:plugins {:legend {:display display-legend}}}
                :pie {:maintainAspectRatio false}
                :doughnut {:maintainAspectRatio false})}))

(defn create-chart-control [canvas data]
  (let [[_ type] (:shashurup.quf/hint (meta data))
        input (chart-input data type)]
    (js/Chart. canvas (clj->js input))))

(defmethod render :chart [subj]
  (.log js/console "render chart")
  (let [canvas (crate/html [:canvas])]
    (defer #(create-chart-control canvas subj))
    [:div.quf-medium-sized canvas]))

(defmethod re-render :chart [subj target]
  (.log js/console "re-render chart")
  (when-let [chart (some-> target
                           (.getElementsByTagName "canvas")
                           first
                           js/Chart.getChart)]
    (let [[_ type] (:shashurup.quf/hint (meta subj)) 
          data (chart-data subj type)]
      (doseq [[idx v] (map-indexed vector (:datasets data))]
        (set! (.-data (aget (.-datasets (.-data chart)) idx))
              (clj->js (:data v))))
      (.update chart)
      true)))

(u/set-module-loaded!)
