(ns shashurup.quf.chartjs
  (:require [shashurup.quf.render :refer [render re-render defer]]
            [shashurup.quf.utils :as u]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [chartjs]))

(u/begin-module-load! :chartjs)

(defn chart-data [subj type]
  (cond
    (and (map? subj)
         (:datasets subj))
    subj

    (and (coll? subj)
         (not (coll? (first subj)))
         (#{:line :bar} type))
    {:datasets
     [{:data (if (= type :line)
               (map-indexed #(hash-map :x (str %1) :y %2) subj)
               (for [x subj] {:x (str x) :y x}))}]}

    :else
    {:datasets [{:data subj}]}))

(defn chart-input [data type]
  (let [data (chart-data data type)
        display-legend (->> (:datasets data)
                            (some :label))]
    (.log js/console display-legend)
    {:type (name type)
     :data data
     :options (condp = type
                :bar {:plugins {:legend {:display display-legend}}}
                :line {:plugins {:legend {:display display-legend}}}
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
  (when-let [canvas (first (.getElementsByTagName target "canvas"))]
    (when-let [chart (js/Chart.getChart canvas)]
      (set! (.-labels (.-data chart))
            (clj->js (map first subj)))
      (set! (.-data (aget (.-datasets (.-data chart)) 0))
            (clj->js (map second subj)))
      (.update chart)
      true)))

(u/set-module-loaded!)
