(ns shashurup.quf.chartjs
  (:require [shashurup.quf.render :refer [render]]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [cljsjs.chartjs :as chart]))

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
                                            :backgroundColor (first colors)})
      :line (compose-input skeleton dataset {:scales {:y {:beginAtZero true}}
                                             :borderColor (first colors)})
      :pie (compose-input skeleton
                          (add-colors dataset (count data))
                          {:maintainAspectRatio false})
      :doughnut (compose-input skeleton
                               (add-colors dataset (count data))
                               {:maintainAspectRatio false})
      :scatter {:type type
                :data {:datasets [{:data data
                                   :backgroundColor (first colors)}]}}
      {})))

(defn create-chart-control [canvas data]
  (let [[_ type] (:shashurup.quf/hint (meta data))]
    (js/Chart. canvas (clj->js (chart-input data type)))))

(defmethod render :chart [subj]
  (fn [target]
    (let [c (crate/html [:div.quf-medium-sized [:canvas]])]
      (gdom/appendChild target c)
      (create-chart-control (gdom/getFirstElementChild c) subj))))
