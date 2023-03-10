(ns rackushka.db
  (:require
   [clojure.string :as s]
   [clojure.java.jdbc :as jdbc]
   [monger.core :as mg]
   [monger.collection :as mc]
   [rackushka.secrets :as secrets]))

(def ^:dynamic *current*)

(def ^:dynamic *book* {})

(defn use [subj]
  (def ^:dynamic *current* subj))

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

(defn- resolve-creds [db]
  (let [pwd (:password db)]
    (if (map? pwd)
      (assoc db :password (secrets/find pwd))
      db)))

(defn- parse-pg-uri [subj]
  (let [uri (java.net.URI. subj)
        user-info (.getUserInfo uri)
        parts [[:dbtype "postgresql"]
               [:host (.getHost uri)]
               [:port (let [p (.getPort uri)]
                        (when (> p 0) p))]
               [:dbname (let [p (.getPath uri)]
                          (when (not-empty p)
                            (s/replace p #"^/" "")))]
               [:user (when (not-empty user-info)
                        (first (s/split user-info #":")))]
               [:password (when (not-empty user-info)
                            (second (s/split user-info #":")))]]]
    (->> parts
         (filter second)
         (into {}))))

(defmulti query-by-map {:private true} (fn [db &_] (:dbtype db)))

(defmulti query-by-uri {:private true} (fn [uri &_] (.getScheme (java.net.URI. uri))))

(defmulti query (fn [db &_] (class db)))

(defmethod query-by-uri "mongodb" [uri & args]
  (apply mc/find-maps (:db (mg/connect-via-uri uri)) args))

(defmethod query-by-uri "postgresql" [uri & args]
  (apply query-by-map (parse-pg-uri uri) args))

(defmethod query-by-map "mongodb" [db & args]
  (let [c (mg/connect (resolve-creds db))]
    (apply mc/find-maps (mg/get-db c (:dbname db)) args)))

(defmethod query-by-map :default [db & args]
  (let [handle (fn [rset]
                 (let [meta (make-meta rset)]
                   (with-meta 
                     (jdbc/metadata-result rset)
                     meta)))]
    (jdbc/db-query-with-resultset (resolve-creds db) args handle)))

(defmethod query-by-uri :default [uri & args]
  (apply query {:connection-uri uri} args))

(defmethod query String [db & args]
  (apply query-by-uri db args))

(defmethod query clojure.lang.IPersistentMap [db & args]
  (apply query-by-map db args))

(defmethod query :default [db & args]
  (throw (Exception. "db must be either string or map")))

(defn q [& args]
  (let [[db args] (if (keyword? (first args))
                    [(get *book* (first args)) (rest args)]
                    [*current* args])]
    (apply query db args)))
