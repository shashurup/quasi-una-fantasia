(ns shashurup.quf.monitoring
  (:require [shashurup.quf.linux :as linux]
            [shashurup.quf.chart :as chart]
            [shashurup.quf.view :as v]
            [clojure.string :as s]))

(defn- delta [subj interval]
  (let [c (count subj)]
    (if (> c 1)
      (/ (- (last subj)
            (nth subj (- c 2))) interval)
      0)))

(defn- cur [subj _]
  (last subj))

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

(def sources
  {:mem-free-kb [mem-free-kb cur]
   :mem-free [mem-free cur]
   :cpu [cpu-busy delta]})

(defn- update-cache [subj keys]
  (reduce (fn [m k]
            (let [f (first (sources k))]
              (update m k (fnil conj []) (f))))
          subj
          keys))

(defn- update-state [state cache keys interval samples]
  (let [upd (fn [m k]
              (let [f (second (sources k))]
                (update m k (fnil conj []) (f (cache k) interval))))]
    (update-vals (reduce upd state keys)
                 (fn [v]
                   (let [c (count v)]
                     (if (> c samples)
                       (subvec v (- c samples))
                       v))))))

(defn plot
  ([subj] (plot subj 1000 120))
  ([subj interval] (plot subj interval 120))
  ([subj interval sample-count]
   (let [int-secs (/ interval 1000)
         go (fn [state]
              (loop [cache {}]
                (let [cache (update-cache cache subj)]
                  (swap! state #(update-state %
                                              cache
                                              subj
                                              int-secs
                                              sample-count))
                  (Thread/sleep interval)
                  (recur cache))))]
     (v/start go chart/line))))
