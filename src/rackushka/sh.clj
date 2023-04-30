(ns rackushka.sh
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [rackushka.fs :as fs])
  (:import [java.lang ProcessBuilder]))

;; clojure.java.shell/sh always create a pipe for stdin
;; this ins't always what we need
;; for instance, ag thinks it needs to search stdin in this case
;; so here goes our own implementation

(defn- start-process [cmd dir]
  (let [p (-> (ProcessBuilder. (into-array String cmd))
              (.directory (io/as-file dir))
              (.redirectInput (io/as-file "/dev/null"))
              .start)]
    {:out (io/reader (.getInputStream p))
     :err (io/reader (.getErrorStream p))
     :wait #(.waitFor p)}))

(defn ! [& args]
  (let [args (if (> (count args) 1)
               args
               (remove empty? (s/split (first args) #"\s")))]
    (let [{:keys [out err wait]} (start-process args fs/*cwd*)]
      (let [output (vec (line-seq out))
            error (future (slurp err))
            exit (wait)]
        (when (not-empty @error)
          (print @error))
        (if (= exit 0)
          (with-meta output {:rackushka/hint :text})
          (throw (Exception. (str "Exit code " exit))))))))
