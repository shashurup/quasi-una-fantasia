(ns rackushka.data
  (:require [clojure.xml :as xml]
            [cheshire.core :as json]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn as-hiccup [{tag :tag attrs :attrs content :content}]
  (let [rest (map #(if (map? %) (as-hiccup %) %) content)]
    (vec (if attrs
           (cons tag (cons attrs rest))
           (cons tag rest)))))

(defn read-json [url]
  (with-open [r (io/reader url)]
    (json/parse-stream r)))

(defn read-xml [url]
  (as-hiccup (xml/parse url)))

(defn read-lines [url]
  (with-meta
    (with-open [r (io/reader url)]
      (doall (line-seq r)))
    {:rackushka/hint :text}))

(defn read-csv [url]
  (with-meta
    (with-open [r (io/reader url)]
      (doall (csv/read-csv r)))
    {:rackushka/hint :table}))
