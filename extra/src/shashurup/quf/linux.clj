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
