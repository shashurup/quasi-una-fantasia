(ns shashurup.quf.db
  (:require
   [clojure.string :as s]
   [clojure.set :as set]
   [clojure.java.jdbc :as jdbc]
   [monger.core :as mg]
   [monger.collection :as mc]
   [monger.credentials :as mcreds]
   [shashurup.quf.secrets :as secrets]))

(def ^:dynamic *current*)

(def ^:dynamic *book*)

(defn c
  "Sets *current* database, used when it is absent in other functions.
   subj - could be:
          a map with :dbtype, :dbname, :host, :port, :user, :password
          a connection string, database specific
          a keyword to lookup a database in the *book*"
  [subj]
  (def ^:dynamic *current* (if (keyword? subj)
                             (get *book* subj)
                             subj)))

(defn set-book! [subj]
  (def ^:dynamic *book* subj))

(defn- make-meta [rset]
  (let [rset-meta (.getMetaData rset)]
    {:shashurup.quf/hint [:table
                      (vec (for [idx (range 1 (inc (.getColumnCount rset-meta)))]
                             (let [col-label (.getColumnLabel rset-meta idx)
                                   col-type (.getColumnTypeName rset-meta idx)]
                               {:key (dec idx)
                                :title col-label
                                :type (keyword "shashurup.quf.db"
                                               (s/lower-case col-type))})))]}))

(defn- resolve-creds [db]
  (let [pwd (:password db)]
    (if (map? pwd)
      (assoc db :password (secrets/lookup pwd))
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

(defmulti query-by-map {:private true} (fn [db & _] (:dbtype db)))

(defmulti query-by-uri {:private true} (fn [uri & _] (.getScheme (java.net.URI. uri))))

(defmulti query (fn [db & _] (class db)))

(defmethod query-by-uri "mongodb" [uri & args]
  (apply mc/find-maps (:db (mg/connect-via-uri uri)) args))

(defmethod query-by-uri "postgresql" [uri & args]
  (apply query-by-map (parse-pg-uri uri) args))

(defmethod query-by-map "mongodb" [db & args]
  (let [{auth-src :auth-source
         user :user
         pwd :password
         dbname :dbname
         host :host
         port :port} (resolve-creds db)
        sa (mg/server-address host (or port 27017))
        opts (mg/mongo-options db)
        c (if user
            (mg/connect sa opts (mcreds/create user
                                               (or auth-src dbname)
                                               pwd))
            (mg/connect sa opts))]
    (apply mc/find-maps (mg/get-db c dbname) args)))

(defmethod query-by-map :default [db & args]
  (let [handle (fn [rset]
                 (let [meta (make-meta rset)]
                   (with-meta 
                     (subvec
                      (jdbc/metadata-result rset {:as-arrays? true}) 1)
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

(defn- preprocess [args]
  (if (keyword? (first args))
    [(get *book* (first args)) (rest args)]
    [*current* args]))

(defn q
  "Query a database, args are:
   database query param1 param2 ....
   (q :sales-db \"select * from order where id = ?\" 123)
   database - optional, a keyword to lookup in the *book*
              *current* is used when ommited
   query - for SQL databases it is a query string
           parameter placeholder is ?
           or db specific structure for other databases
           for instance, for Mongo it could be something like:
             \"coll\" {:field1 val} [:field2 :field3]
  "
  [& args]
  (let [[db args] (preprocess args)]
    (apply query db args)))

(defn- transform [subj renames]
  (map #(set/rename-keys (select-keys % (keys renames))
                         renames) subj))

(defn dn
  "Lists database schemas. Optional argument is a database to query."
  [& args]
  (let [[db _] (preprocess args)]
    (with-meta
      (transform 
       (jdbc/with-db-metadata [m (resolve-creds db)]
         (jdbc/metadata-query (.getSchemas m)))
       {:table_schem :schema})
      {:shashurup.quf/hint :table})))

(defn df
  "Lists database functions, args are:
   database schema function
   database - optional, database to connect to, see q for details
   schema - optional schema name pattern
   function - function name pattern
   % is used as wildcard"
  [& args]
  (let [[db args] (preprocess args)
        [schema fun] (if (> (count args) 1)
                         args
                         [nil (first args)])]
    (with-meta
      (transform 
       (jdbc/with-db-metadata [m (resolve-creds db)]
         (jdbc/metadata-query (.getFunctions m nil schema fun)))
       {:function_schem :schema
        :function_name :function
        :remarks :remarks})
      {:shashurup.quf/hint :table})))

(defn dt
  "Lists database tables, args are:
   database schema function
   database - optional, database to connect to, see q for details
   schema - optional schema name pattern
   table - table name pattern
   % is used as wildcard"
  [& args]
  (let [[db args] (preprocess args)
        [schema table] (if (> (count args) 1)
                         args
                         [nil (first args)])]
    (with-meta
      (transform 
       (jdbc/with-db-metadata [m (resolve-creds db)]
         (jdbc/metadata-query (.getTables m nil schema table nil)))
       {:table_type :type
        :table_schem :schema
        :table_name :table})
      {:shashurup.quf/hint :table})))

(defn d
  "Describe database table, args are:
   database schema function
   database - optional, database to connect to, see q for details
   schema - optional schema name pattern
   table - table name
   % is used as wildcard"
  [& args]
  (let [[db args] (preprocess args)
        [schema table] (if (> (count args) 1)
                         args
                         [nil (first args)])]
    (with-meta
      (transform 
       (jdbc/with-db-metadata [m (resolve-creds db)]
         (jdbc/metadata-query (.getColumns m nil schema table nil)))
       {:table_schem :schema
        :table_name :table
        :column_name :column
        :type_name :type
        :column_size :size
        :is_nullable :null})
      {:shashurup.quf/hint :table})))
