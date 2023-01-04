(ns rackushka.db
  (:require
   [clojure.string :as s]
   [clojure.java.jdbc :as jdbc]
   [monger.core :as mg]
   [monger.collection :as mc]))

(def ^:dynamic *current*)

(defn use [subj]
  (let [new-val (if (string? subj)
                  {:connection-uri subj}
                  subj)]
    (def ^:dynamic *current* new-val)))

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
  (let [db (if-let [uri (:connection-uri desc)]
             (:db (mg/connect-via-uri uri))
             (let [c (mg/connect desc)]
               (mg/get-db c (:dbname desc))))]
    (apply mc/find-maps db args)))

(defn- mongo? [subj]
  (or (= (:dbtype subj) "mongodb")
      (s/starts-with? (:connection-uri subj "") "mongodb:")))

(defn q [& args]
  (let [[conn args] (if (map? (first args))
                      [(first args) (rest args)]
                      [*current* args])]
    (if (mongo? conn)
      (mongo-query conn args)
      (jdbc-query conn args))))
