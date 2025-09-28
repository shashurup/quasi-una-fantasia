(ns shashurup.quf.db
  (:require
   [clojure.string :as s]
   [clojure.set :as set]
   [clojure.java.jdbc :as jdbc]
   [shashurup.quf.secrets :as secrets]
   [shashurup.quf.response :as resp])
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
                          :column_def :default})

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
    (map 
     add-object-key
     (rename-keys
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
  (rename-keys
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (map render-type
          (jdbc/metadata-query (.getColumns m nil schema table nil))))
   field-map))

(defn- get-fn-args [db schema function]
  (rename-keys
   (jdbc/with-db-metadata [m (resolve-creds db)]
     (jdbc/metadata-query (.getFunctionColumns m nil schema function nil)))
   field-map))

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
      (empty? arg) (resp/hint
                    (for [sch (get-schemas db)
                          :let [pattern (str sch ".*")]]
                      {:name sch
                       :key sch
                       :children (with-meta []
                                   {:shashurup.quf/range {:more? true}
                                    :shashurup.quf/more `(d ~db ~sch)})})
                    [:tree {:actions {:default `(d ~db)}}])
      (pattern? arg) (let [[schema obj] (parse-arg arg)
                           data (get-objects db schema obj)]
                       (resp/hint (map name-with-schema data)
                                  [:tree {:actions {:default `(d ~db)}}]))
      (string? arg) (let [[schema table] (parse-arg arg)]
                      (if table
                        (resp/hint (concat (get-columns db schema table)
                                           (get-fn-args db schema table))
                                   [:table [:column :data-type :null
                                            :default :remarks]])
                        (resp/hint (get-objects db schema "%")
                                   [:table [:type :name :remarks]]))))))
