(ns shashurup.quf.data
  (:require [clojure.xml :as xml]
            [cheshire.core :as json]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [shashurup.quf.response :as resp]))

(defn as-hiccup [{tag :tag attrs :attrs content :content}]
  (let [rest (map #(if (map? %) (as-hiccup %) %) content)]
    (vec (if attrs
           (cons tag (cons attrs rest))
           (cons tag rest)))))

(defn from-json [subj]
  (cond
    (coll? subj) (json/parse-string (s/join "\n" subj))
    (instance? java.io.Reader subj) (json/parse-stream subj)
    :else (with-open [r (io/reader subj)]
            (json/parse-stream r))))

(defn from-xml [subj]
  (as-hiccup (xml/parse subj)))

(defn as-text [subj]
  (resp/hint
    (if (instance? java.io.Reader subj)
      (line-seq subj)
      (with-open [r (io/reader subj)]
        (doall (line-seq r))))
    :text))

(defn from-csv [subj]
  (resp/hint
    (cond
      ;; (coll? subj) (csv/read-string (s/join "\n" subj))
      (instance? java.io.Reader subj) (csv/read-csv subj)
      :else (with-open [r (io/reader subj)]
              (doall (csv/read-csv r))))
    :table))
