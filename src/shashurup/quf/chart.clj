(ns shashurup.quf.chart
  (:require [clojure.math :as math]
            [shashurup.quf.events :as events]))

(def grid [10 15 20 25 30 40 50 60 70 80 90 100])

(defn- snap-to-grid
  ([subj] (snap-to-grid subj 0))
  ([subj n]
    (let [m (* subj (math/pow 10 (- n)))]
      (cond
        (> m (last grid)) (snap-to-grid subj (inc n))
        (< m (first grid)) (snap-to-grid subj (dec n))
        :else (->> grid
                   (map #(vector % (abs (- % m))))
                   (sort-by second)
                   ffirst
                   (* (math/pow 10 n)))))))

(defn normalize [data]
  (for [x data]
    (if (coll? x) x [(str x) x])))

(defn show [data type] (with-meta (normalize data) {:shashurup.quf/hint [:chart type]}))

(defn bar [data] (show data :bar))

(defn line [data] (show data :line))

(defn pie [data] (show data :pie))

(defn doughnut [data] (show data :doughnut))

(defn scatter [data] (with-meta data {:shashurup.quf/hint [:chart :scatter]}))

(defn histogram
  ([data] (histogram 11 data))
  ([n data]
   (let [first-val (apply min data)
         step (snap-to-grid (/ (- (apply max data) first-val) n))
         start (* (quot first-val step) step)]
     (->> (reduce (fn [a v]
                    (let [k (quot (- v start) step)]
                      (update a k #(inc (or % 0))))) {} data)
          (map (fn [[k v]] [(+ start (* step k)) v]))
          (sort-by first)
          bar))))

(events/push {:type :require :ns "shashurup.quf.chartjs"})
