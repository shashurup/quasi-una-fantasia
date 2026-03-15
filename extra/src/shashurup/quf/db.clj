(ns shashurup.quf.db
  (:require
   [clojure.string :as s]
   [clojure.set :as set]
   [clojure.java.jdbc :as jdbc]
   [shashurup.quf.secrets :as secrets]
   [shashurup.quf.ui :as ui])
  (:import [java.sql Types]))

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

(defn get-db-type [db & _]
  (if (string? db)
    (.getScheme (java.net.URI. db))
    (:dbtype db)))

(defn query [db & args]
  (let [db (if (string? db)
             {:connection-uri db}
             db)
        handle (fn [rset]
                 (let [meta (make-meta rset)]
                   (with-meta 
                     (subvec
                      (jdbc/metadata-result rset {:as-arrays? true}) 1)
                     meta)))]
    (jdbc/db-query-with-resultset (resolve-creds db) args handle)))

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

; (defmulti get-view-query {:private true} get-db-type)
(defmulti get-view-query get-db-type)

(defmethod get-view-query :default [db schema view]
  "View query is not available")

(defmethod get-view-query "postgresql" [db schema view]
  (ffirst (query db "select definition from pg_views
                     where schemaname = ? and viewname = ?" schema view)))

(defmulti get-function-bodies get-db-type)

(defmethod get-function-bodies :default [db schema func]
  "Function bodies are not available")

(defmethod get-function-bodies "postgresql" [db schema func]
  (map first
       (query db "select pg_get_functiondef(p.oid)
                  from pg_proc p
                  join pg_namespace n on p.pronamespace = n.oid
                  where nspname = ? and proname= ? " schema func)))

(defn- rename-keys [subj renames]
  (map #(set/rename-keys (select-keys % (keys renames))
                         renames) subj))

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
                          :data-type :data-type
                          :remarks :remarks
                          :column_name :column
                          :is_nullable :null
                          :column_def :default
                          })

(def ^:private size1 #{java.sql.Types/VARCHAR
                       java.sql.Types/NVARCHAR
                       java.sql.Types/LONGVARCHAR
                       java.sql.Types/LONGNVARCHAR})

(def ^:private size2 #{java.sql.Types/DECIMAL java.sql.Types/NUMERIC})

(defn- render-type [{:keys [:data_type :type_name
                            :column_size :decimal_digits] :as subj}]
  (let [type-str (cond
                   (and (not= column_size java.lang.Integer/MAX_VALUE)
                        (size1 data_type)) (format "%s(%s)"
                                                   type_name
                                                   column_size)
                   (and (size2 data_type)
                        decimal_digits) (format "%s(%s,%s)"
                                                type_name
                                                column_size
                                                decimal_digits)
                   (= data_type java.sql.Types/ARRAY) (str type_name "[]")
                   :else type_name)]
    (assoc subj :data-type type-str)))

(defn- add-object-key [{:keys [:name :schema] :as subj}]
  (assoc subj :key (str schema "." name)))

(defn- get-objects [db schema obj]
  (jdbc/with-db-metadata [m (resolve-creds db)]
    (->> (rename-keys
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
          field-map)
         (map add-object-key)
         (group-by :key)
         vals
         (map first)
         (sort-by :key))))

(defn- get-columns [db schema table]
  (rename-keys
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (map render-type
          (jdbc/metadata-query (.getColumns m nil schema table nil))))
   field-map))

(defn- get-indicies [db schema table]
  (rename-keys
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (map render-type
          (jdbc/metadata-query (.getIndexInfo m nil schema table false false))))
   {:index_name :name :column_name :column :filter_condition :filter}))

(defn- get-primary-keys [db schema table]
  (rename-keys
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (map render-type
          (jdbc/metadata-query (.getPrimaryKeys m nil schema table))))
   {:pk_name :name :column_name :column}))

(defn- get-foreign-keys [db schema table]
  (rename-keys
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (map render-type
          (jdbc/metadata-query (.getImportedKeys m nil schema table))))
   {:fk_name :name
    :fkcolumn_name :column
    :pktable_schem :schema2
    :pktable_name :table2
    :pkcolumn_name :column2}))

(defn- get-fn-args [db schema function]
  (rename-keys
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (jdbc/metadata-query (.getFunctionColumns m nil schema function nil)))
   field-map))

(defn- get-object-details [db schema object]
  (let [cols (ui/table [:column :data-type :null :default :remarks]
                       (get-columns db schema object))]
    (if (empty? cols)
      (ui/code (get-function-bodies db schema object) "pgsql" "sql")
      (if-let [view (get-view-query db schema object)]
        (ui/sequence [cols
                      (ui/html [:h3 "Query"])
                      (ui/code view "sql")])
        (let [ind (ui/table [:name :column :filter]
                            (get-indicies db schema object))
              pks (ui/table [:name :column]
                            (get-primary-keys db schema object))
              fks (ui/table [:name :column :schema2 :table2 :column2]
                            (get-foreign-keys db schema object))]
          (ui/sequence [cols
                        (ui/html [:h3 "Primary keys"])
                        pks
                        (ui/html [:h3 "Indexes"])
                        ind
                        (ui/html [:h3 "Foreign keys"])
                        fks]))))))

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

(defn d
  "Describe database table, args are:
   string - db object name with schema to describe
            or a pattern with * and ?
   without arguments shows object tree.
   "
  [& args]
  (let [[db _] (preprocess args)
        arg (first (filter string? args))]
    (cond
      (empty? arg) (ui/tree
                    {:actions {:default `(d ~db)}}
                    (for [sch (get-schemas db)
                          :let [pattern (str sch ".*")]]
                      {:name sch
                       :key sch
                       :children (with-meta []
                                   {:shashurup.quf/range {:more? true}
                                    :shashurup.quf/more `(d ~db ~sch)})}))
      (pattern? arg) (let [[schema obj] (parse-arg arg)
                           data (get-objects db schema obj)]
                       (ui/tree {:actions {:default `(d ~db)}}
                                (map name-with-schema data)))
      (string? arg) (let [[schema table] (parse-arg arg)]
                      (if table
                        (get-object-details db schema table)
                        (ui/table [:type :name :remarks]
                                  (get-objects db schema "%")))))))

(defn- make-table-name [subj]
  (str \" (:table_schem subj) "\".\"" (:table_name subj) \"))

(defn- search-tables [meta schema table]
  (jdbc/metadata-query (.getTables meta nil schema table
                                   (into-array String ["TABLE"]))))

(defn- guess-primary-key [meta {schema :table_schem
                                table :table_name}]
  (let [pks (jdbc/metadata-query
             (.getPrimaryKeys meta nil schema table))]
    (if (empty? pks)
      (let [cols (jdbc/metadata-query (.getColumns meta nil schema table nil))]
        (->> cols
             (map :column_name)
             (filter #(or (= % "id")
                          (= % (str table "_id"))))
             first))
      (when (= (count pks) 1)
        (:column_name (first pks))))))

(defn- guess-table-name [meta table]
  (let [matches (search-tables meta "%" (str "%" table "%"))
        exact-matches (filter #(= (:table_name %)
                                  (str table))
                              matches)]
    (cond
      (empty? matches) (print "No matches for" table)
      (= (count exact-matches) 1) (first exact-matches)
      (> (count matches) 1) (print (map make-table-name matches)
                                   "which one?")
      :else (first matches)))
  )

(defn- explore [db args]
  (when-let [table (first (filter symbol? args))]
    (jdbc/with-db-metadata [m (resolve-creds db)]
      (when-let [table (guess-table-name m table)]
        (let [pk (guess-primary-key m table)]
          (str "select * from "
               (make-table-name table)
               (when pk
                 (str " order by \"" pk "\" desc"))
               " limit 10"))))))

(defmacro e
  "Explore database contents, args are:
   - symbol, table name"
  [& args]
  (let [[db# args#] (preprocess args)]
    (when-let [sql# (explore db# args#)]
      (println sql#)
      `(query ~db# ~sql#))))

;; java.sql.Date inherits java.util.Date
;; wich causes pr to render it as #inst
;; wich in turn hides the nature of DATE
;; field data type
(defmethod print-method java.sql.Date [d w]
  (.write w "#object [java.sql.Date \"")
  (.write w (.toString d))
  (.write w "\"]"))
