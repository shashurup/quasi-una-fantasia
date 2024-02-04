(ns shashurup.quf.base16
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :refer [split-lines]]
            [clj-yaml.core :as yaml]))

(def index-url "https://github.com/chriskempson/base16-schemes-source/raw/main/list.yaml")

(defn clone-repo [idx url]
  (println "Cloning " url)
  (sh "git" "clone" url (str "base16/" idx) :env {"GIT_TERMINAL_PROMPT" "false"}))

(defn clone-themes []
  (println "Downloading theme index from " index-url)
  (with-open [r (io/reader index-url)]
    (let [index (yaml/parse-stream r)]
      (doall (map-indexed clone-repo (vals index))))))

(defn read-theme [filename]
  (with-open [r (io/reader filename)]
    (try
      (let [scheme (yaml/parse-stream r)]
        (when (:scheme scheme)
          (into {} scheme)))
      (catch Exception _))) )

(defn -main [filename]
  (clone-themes)
  (let [result (sh "find" "base16" "-name" "*.yaml" "-o" "-name" "*.yml")
        files (split-lines (:out result))]
    (with-open [w (io/writer filename)]
      (binding [*out* w]
        (pr (->> files
                 (map read-theme)
                 (remove nil?)))))))
