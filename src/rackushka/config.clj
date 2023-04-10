(ns rackushka.config
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn- absolute-path [name]
  (let [cfg-home (or (System/getenv "XDG_CONFIG_HOME")
                     (str (System/getenv "HOME") "/.config"))]
    (str cfg-home "/rackushka/" name)))

(defn read [name]
  (let [path (absolute-path name)]
    (when (.exists (io/as-file path))
      (with-open [r (io/reader path)]
        (edn/read (java.io.PushbackReader. r))))))

(defn write [name contents]
  (let [path (absolute-path name)]
    (io/make-parents path)
    (with-open [w (io/writer path)]
      (binding [*out* w]
        (pr contents)))))
