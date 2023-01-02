(ns rackushka.db
  (:require
   [clojure.string :as s]
   [clojure.java.jdbc :as jdbc]
   [monger.core :as mg]
   [monger.collection :as mc]))

(def ^:dynamic *current*)

(defn use [subj]
  (def *current* subj))

(defn make-meta [rset]
  (let [rset-meta (.getMetaData rset)]
    {:rackushka/hint [:table
                      (vec (for [idx (range 1 (inc (.getColumnCount rset-meta)))]
                             (let [col-label (.getColumnLabel rset-meta idx)
                                   col-type (.getColumnTypeName rset-meta idx)]
                               {:key (keyword (s/lower-case col-label))
                                :title col-label
                                :type (keyword "rackushka.db"
                                               (s/lower-case col-type))})))]}))

(defn- jdbc-query [conn args]
  (let [handle (fn [rset]
                 (let [meta (make-meta rset)]
                   (with-meta 
                     (jdbc/metadata-result rset)
                     meta)))]
    (jdbc/db-query-with-resultset conn args handle)))

(defn- mongo-query [desc args]
  (let [c (mg/connect desc)
        d (mg/get-db c (:dbname desc))]
    (apply mc/find-maps d args)))

(defn q [& args]
  (let [[conn args] (if (map? (first args))
                      [(first args) (rest args)]
                      [*current* args])]
    (if (= (:dbtype conn) "mongodb")
      (mongo-query conn args)
      (jdbc-query conn args))))
