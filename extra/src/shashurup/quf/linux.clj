(ns shashurup.quf.linux
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [java.nio.file Files Paths]))

(defn- read-zero-sized-file
  "Read files which cannot be read with BufferedStream
   Because they cannot be seeked."
  [filename]
  (-> filename
      (Paths/get (into-array String []))
      Files/lines
      .iterator
      iterator-seq))

(defn proc-meminfo []
  (->> (read-zero-sized-file "/proc/meminfo")
       (map (fn [line]
              (let [[n v] (s/split line #":")]
                [(s/trim n)
                 (parse-long (first (s/split (s/trim v) #" +")))]
                )))
       (into {})))

(defn proc-stat []
  (->> (read-zero-sized-file "/proc/stat")
       (map (fn [line]
              (let [[n & r] (s/split line #" +")]
                [n (mapv parse-long r)])))
       (into {})))

(defn proc-net-dev []
  (->> (read-zero-sized-file "/proc/net/dev")
       (drop 2)
       (map #(s/split % #":"))
       (map (fn [[i ms]]
              [(s/trim i)
               (mapv parse-long
                     (s/split (s/trim ms) #" +"))]))
       (into {})))

(defn proc-diskstats []
  (->> (read-zero-sized-file "/proc/diskstats")
       (map s/trim)
       (map #(s/split % #" +"))
       (map (fn [x]
              [(nth x 2)
               (mapv parse-long
                     (drop 3 x))]))
       (into {})))

(defn proc-pid-stat [pid]
  (let [[pid cmd state & rest] (-> (str "/proc/" pid "/stat")
                                   read-zero-sized-file
                                   first
                                   (s/split #" "))]
    (into [(parse-long pid) cmd state]
          (map parse-long rest))))

(defn proc-pid-statm [pid]
  (let [vals (-> (str "/proc/" pid "/statm")
                 read-zero-sized-file
                 first
                 (s/split #" "))]
    (mapv parse-long vals)))

(defn proc-pid-io [pid]
  (->> (str "/proc/" pid "/io")
       read-zero-sized-file
       (map #(s/split % #":"))
       (map (fn [[k v]] [k (parse-long (s/trim v))]))
       (into {})))
