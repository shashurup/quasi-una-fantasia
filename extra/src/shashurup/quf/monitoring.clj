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

(defn- buffers-kb []
  (get (linux/proc-meminfo) "Buffers"))

(defn- buffers []
  (let [total (get (linux/proc-meminfo) "MemTotal")]
    (/ (* (buffers-kb) 100) total)))

;; TODO get page size from the system
(defn- proc-mem-kb' [pid]
  (* (second (linux/proc-pid-statm pid)) 4))

(defn- proc-mem' [pid]
  (let [total (get (linux/proc-meminfo) "MemTotal")]
    (/ (* (proc-mem-kb' pid) 100) total)))

;; TODO get tick size from the system
(defn- cpu-busy
  ([] (cpu-busy nil))
  ([n]
   (let [ps (linux/proc-stat)
         cpu-count (->> ps
                        keys
                        (filter #(s/starts-with? % "cpu"))
                        count
                        dec)
         vals (get ps (str "cpu" n))]
     (/ (reduce + (concat (subvec vals 0 3)
                          (subvec vals 5)))
        cpu-count))))

(defn- proc-busy [pid]
  (let [ps (linux/proc-pid-stat pid)]
    (+ (nth ps 13) (nth ps 14))))

(defn- interface-received [subj]
  (first ((linux/proc-net-dev) subj)))

(defn- interface-transmitted [subj]
  (nth ((linux/proc-net-dev) subj) 8))

(defn interfaces []
  (keys (linux/proc-net-dev)))

(defn- disk-reads [subj]
  (first ((linux/proc-diskstats) subj)))

(defn- disk-writes [subj]
  (nth ((linux/proc-diskstats) subj) 4))

(defn disks []
  (keys (linux/proc-diskstats)))

(defn wrap-delta [f]
  (fn
    ([] [0 (f)])
    ([prev interval]
     (let [v (f)]
       [(/ (- v prev) interval) v]))))

(def cpu ["cpu" (wrap-delta cpu-busy)])

(defn cpu-core [n]
  [(str "cpu core " n)
   (wrap-delta #(cpu-busy n))])

(defn proc-cpu [pid]
  [(str pid " cpu (%)")
   (wrap-delta #(proc-busy pid))])

(def free-mem ["free mem (%)" mem-free])

(def free-mem-kb ["free mem (kb)" mem-free-kb])

(def mem-buffers ["mem buffers (%)" buffers])

(def mem-buffers-kb ["mem buffers (kb)" buffers-kb])

(defn proc-mem [pid]
  [(str pid " mem (%)") #(proc-mem' pid)])

(defn proc-mem-kb [pid]
  [(str pid " mem (kb)") #(proc-mem-kb' pid)])

(defn proc-read [pid]
  [(str pid " read (bps)")
   (wrap-delta #(get (linux/proc-pid-io pid) "read_bytes"))])

(defn proc-written [pid]
  [(str pid " written (bps)")
   (wrap-delta #(get (linux/proc-pid-io pid) "write_bytes"))] )

(defn received [interface]
  [(str interface " received (bps)")
   (wrap-delta #(interface-received interface))])

(defn transmitted [interface]
  [(str interface " transmitted (bps)")
   (wrap-delta #(interface-transmitted interface))])

(defn reads [disk]
  [(str disk " read (ops)")
   (wrap-delta #(disk-reads disk))])

(defn writes [disk]
  [(str disk " writes (ops)")
   (wrap-delta #(disk-writes disk))])

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
