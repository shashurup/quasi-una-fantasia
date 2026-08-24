(ns shashurup.quf.chartjs
  (:require [shashurup.quf.render :refer [render re-render defer]]
            [shashurup.quf.utils :as u]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [chartjs]))

(u/begin-module-load! :chartjs)

(def colors ["#114b5f" "#1a936f" "#744253"
             "#70798c" "#aec3b0" "#b36a5e"
             "#9381ff" "#f3c26e" "#da8c5b"
             "#ce5070" "#16f4d0"])

(def default-color "#9CAF88")

(defn compose-input [skeleton dataset options]
  (-> skeleton
      (assoc :options options)
      (assoc-in [:data :datasets] [dataset])))

(defn add-colors [subj amount]
  (assoc subj :backgroundColor (take amount (cycle colors))))

(defn chart-input [data type]
  (let [dataset {:data (map second data)}
        skeleton {:type (name type)
                  :data {:labels (map first data)}}]
    (condp = type
      :bar (compose-input skeleton dataset {:scales {:y {:beginAtZero true}}
                                            :backgroundColor (first colors)
                                            :plugins {:legend {:display false}}})
      :line (compose-input skeleton dataset {:scales {:y {:beginAtZero true}}
                                             :borderColor (first colors)
                                             :plugins {:legend {:display false}}})
      :pie (compose-input skeleton
                          (add-colors dataset (count data))
                          {:maintainAspectRatio false})
      :doughnut (compose-input skeleton
                               (add-colors dataset (count data))
                               {:maintainAspectRatio false})
      :scatter {:type type
                :data {:datasets [{:data data
                                   :backgroundColor (first colors)}]}
                :options {:plugins {:legend {:display false}}}}
      {})))

(defn create-chart-control [canvas data]
  (let [[_ type] (:shashurup.quf/hint (meta data))]
    (js/Chart. canvas (clj->js (chart-input data type)))))

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
