(ns shashurup.quf.monitoring
  (:require [shashurup.quf.linux :as linux]
            [shashurup.quf.chart :as chart]
            [shashurup.quf.view :as v]
            [clojure.string :as s]))

(defn- mem-free-kb []
  (get (linux/proc-meminfo) "MemFree"))

(defn- mem-free []
  (let [total (get (linux/proc-meminfo) "MemTotal")]
    (/ (* (mem-free-kb) 100) total)))

(defn- cpu-busy []
  (let [ps (linux/proc-stat)
        cpu-count (->> ps
                       keys
                       (filter #(s/starts-with? % "cpu"))
                       count
                       dec)
        vals (get ps "cpu")]
    (/ (reduce + (concat (subvec vals 0 3)
                         (subvec vals 5)))
       cpu-count)))

(defn wrap-delta [f]
  (fn
    ([] [0 (f)])
    ([prev interval]
     (let [v (f)]
       [(/ (- v prev) interval) v]))))

(def cpu ["cpu" (wrap-delta cpu-busy)])
(def free-mem ["free mem (%)" mem-free])
(def free-mem-kb ["free mem (kb)" mem-free-kb])

(defn- gather-metrics [subj state interval]
  (let [metrics (for [[k f] subj]
                  (if-let [s (state k)]
                    (let [[v new-state] (f s interval)]
                      [k v new-state])
                    (let [result (f)]
                      (if (coll? result)
                        (let [[v new-state] result]
                          [k v new-state])
                        [k result nil]))))]
    [(into {} (map (fn [[k v _]] [k v])
                   metrics))
     (into {} (->> metrics
                   (map (fn [[k _ s]] [k s]))
                   (filter second)))]))

(defn- update-plot [subj metrics samples]
  (let [upd (fn [m [k v]]
              (update m k (fnil conj []) v))]
    (update-vals (reduce upd subj metrics)
                 (fn [v]
                   (let [c (count v)]
                     (if (> c samples)
                       (subvec v (- c samples))
                       v))))))

(defn plot
  ([subj] (plot subj 1000 60))
  ([subj interval] (plot subj interval 60))
  ([subj interval sample-count]
   (let [int-secs (/ interval 1000)
         go (fn [plot]
              (loop [state {}]
                (let [[metrics state] (gather-metrics subj state int-secs)]
                  (swap! plot #(update-plot % metrics sample-count))
                  (Thread/sleep interval)
                  (recur state))))]
     (v/start go chart/line))))
