(ns shashurup.quf.db
  (:require
   [clojure.string :as s]
   [clojure.set :as set]
   [clojure.java.jdbc :as jdbc]
   [shashurup.quf.secrets :as secrets]
   [shashurup.quf.response :as resp]))

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
                                ;; enable this when some types need special handling
                                ;; :type (keyword "shashurup.quf.db"
                                ;;                (s/lower-case col-type))
                                })))]}))

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

(defmethod query-by-uri "postgresql" [uri & args]
  (apply query-by-map (parse-pg-uri uri) args))

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
           or db specific structure for other databases.
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
    (resp/hint
     (transform 
      (jdbc/with-db-metadata [m (resolve-creds db)]
        (jdbc/metadata-query (.getSchemas m)))
      {:table_schem :schema})
     :table)))

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
    (resp/hint
     (transform 
      (jdbc/with-db-metadata [m (resolve-creds db)]
        (jdbc/metadata-query (.getFunctions m nil schema fun)))
      {:function_schem :schema
       :function_name :function
       :remarks :remarks})
     :table)))

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
    (resp/hint
     (transform 
      (jdbc/with-db-metadata [m (resolve-creds db)]
        (jdbc/metadata-query (.getTables m
                                         nil
                                         schema
                                         table
                                         (into-array String ["TABLE" "VIEW"]))))
      {:table_type :type
       :table_schem :schema
       :table_name :table})
     :table)))

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
    (resp/hint
     (transform 
      (jdbc/with-db-metadata [m (resolve-creds db)]
        (jdbc/metadata-query (.getColumns m nil schema table nil)))
      {:table_schem :schema
       :table_name :table
       :column_name :column
       :type_name :type
       :column_size :size
       :is_nullable :null})
     :table)))


(defn- get-schemas [db]
  (map
   :table_schem
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (jdbc/metadata-query (.getSchemas m)))))

(def ^:private field-map {:table_type :type
                          :table_schem :schema
                          :table_name :name
                          :function_schem :schema
                          :function_name :name
                          :procedure_schem :schema
                          :procedure_name :name
                          :remarks :remarks})

(defn- add-object-key [{:keys [:name :schema] :as subj}]
  (assoc subj :key (str schema "." name)))

(defn- get-objects [db schema obj]
  (jdbc/with-db-metadata [m (resolve-creds db)]
    (map 
     add-object-key
     (transform
      (concat
       (jdbc/metadata-query (.getTables m
                                        nil
                                        schema
                                        obj
                                        (into-array String ["TABLE" "VIEW"])))
       (map #(assoc % :table_type "FUNCTION")
            (jdbc/metadata-query (.getFunctions m nil schema obj)))
       (map #(assoc % :table_type "PROCEDURE")
            (jdbc/metadata-query (.getProcedures m nil schema obj))))
      field-map))))

(defn- get-columns [db schema table]
  (transform 
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (jdbc/metadata-query (.getColumns m nil schema table nil)))
   {:table_schem :schema
    :table_name :table
    :column_name :column
    :type_name :type
    :column_size :size
    :is_nullable :null}))

(defn- get-fn-args [db schema function]
  (transform 
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (jdbc/metadata-query (.getFunctionColumns m nil schema function nil)))
   {:function_schem :schema
    :function_name :table
    :column_name :column
    :type_name :type
    :column_size :size
    :is_nullable :null}))

(defn- pattern? [subj]
  (some #{\* \?} subj))

(defn- parse-arg [arg]
  (let [arg' (-> arg
                 (s/replace #"\*" "%")
                 (s/replace #"\?" "_"))]
    (if (some #{\.} arg')
      (s/split arg' #"\." 2)
      (if (pattern? arg)
        ["%" arg']
        [arg' nil]))))

(defn- name-with-schema [{:keys [:name :schema] :as subj}]
  (assoc subj :name (str schema "." name)))

(defn d2 [& args]
  (let [[db _] (preprocess args)
        arg (first (filter string? args))]
    (cond
      (empty? arg) (resp/hint
                    (for [sch (get-schemas db)
                          :let [pattern (str sch ".*")]]
                      {:name sch
                       :key sch
                       :children (with-meta []
                                   {:shashurup.quf/range {:more? true}
                                    :shashurup.quf/more `(d2 ~db ~sch)})})
                    [:tree {:actions {:default `(d2 ~db)}}])
      (pattern? arg) (let [[schema obj] (parse-arg arg)
                           data (get-objects db schema obj)]
                       (resp/hint (map name-with-schema data)
                                  [:tree {:actions {:default `(d2 ~db)}}]))
      (string? arg) (let [[schema table] (parse-arg arg)]
                      (if table
                        (resp/hint (concat (get-columns db schema table)
                                           (get-fn-args db schema table)) :table)
                        (resp/hint (get-objects db schema "%")
                                   [:table [:type :name :remarks]]))))))
