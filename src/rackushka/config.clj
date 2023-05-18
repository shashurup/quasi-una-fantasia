(ns rackushka.config
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [nrepl.middleware :as mwre]
            [nrepl.misc :as m]
            [nrepl.transport :as t]
            [rackushka.fs :as fs]))

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

(defn ls [name]
  (let [path (absolute-path name)]
    (when (.exists (io/as-file path))
      (map #(fs/relative-path path (:path %)) (fs/files path)))))

(defn wrap-config [h]
  (fn [{:keys [op transport name] :as msg}]
    (condp = op
      "load-config" (t/send transport
                            (m/response-for msg
                                            :status :done
                                            :config (read name)))
      "store-config" (do
                       (write name (:config msg))
                       (t/send transport
                               (m/response-for msg :status :done)))
      "ls-config" (t/send transport
                          (m/response-for msg
                                          :status :done
                                          :names (ls name)))
      (h msg))))

(mwre/set-descriptor! #'wrap-config
                      {:handles
                       {"load-conifg" {:doc "Loads a configuration file"
                                       :requires {"name" "a file name"}}}
                       {"store-config" {:doc "Stores a configuration file"
                                        :requires {"name" "a file name"
                                                   "config" "a configuration"}}}
                       {"ls-config" {:doc "List configuration files"
                                     :requires {"name" "a folder name"}}}})
