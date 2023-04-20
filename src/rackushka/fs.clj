(ns rackushka.fs
  (:import [java.nio.file Files Paths LinkOption]
           [java.nio.file.attribute PosixFilePermission]
           [org.apache.tika Tika])
  (:require [clojure.string :as s]
            [rackushka.data :as data]))

(def permission-map
  {PosixFilePermission/OWNER_READ :owner-read
   PosixFilePermission/OWNER_WRITE :owner-write
   PosixFilePermission/OWNER_EXECUTE :owner-execute
   PosixFilePermission/GROUP_READ :group-read
   PosixFilePermission/GROUP_WRITE :group-write
   PosixFilePermission/GROUP_EXECUTE :group-execute
   PosixFilePermission/OTHERS_READ :others-read
   PosixFilePermission/OTHERS_WRITE :others-write
   PosixFilePermission/OTHERS_EXECUTE :others-execute})

(def no-opts (into-array LinkOption []))

(defn- path? [subj]
  (instance? java.nio.file.Path subj))

(defn- as-path [subj]
  (if (path? subj)
    subj
    (Paths/get (str subj)
               (into-array java.lang.String []))))

(defn- resolve-path [base path]
  (str (.normalize (.resolve (as-path base)
                             (as-path path)))))

(defn relative-path [path other]
  (str (.normalize (.relativize (as-path path)
                                (as-path other)))))

(def ^:dynamic *cwd*)

(defn c
  "Change current directory."
  [path]
  (def ^:dynamic *cwd* (resolve-path *cwd* path)))

(defn- convert-permissions [subj]
  (->> subj
       seq
       (map permission-map)
       (into #{})))

(defn- symlink? [subj]
  (.get subj "isSymbolicLink"))

(defn- convert-attrs [subj]
  {:size (.get subj "size")
   :directory? (.get subj "isDirectory")
   :regular-file? (.get subj "isRegularFile")
   :symlink? (symlink? subj)
   :user (.getName (.get subj "owner"))
   :group (.getName (.get subj "group"))
   :permissions (convert-permissions (.get subj "permissions")) ;TODO figure how to store them
   :modified (.toMillis (.get subj "lastModifiedTime"))
   :created (.toMillis (.get subj "creationTime"))
   :accessed (.toMillis (.get subj "lastAccessTime"))
   }
  )

(defn attrs [path]
  (let [path (as-path path)
        attrs (Files/readAttributes path
                                    "posix:*"
                                    (into-array [LinkOption/NOFOLLOW_LINKS]))]
    (merge (convert-attrs attrs)
           {:path (str path)
            :name (str (.getFileName path))}
           (when (symlink? attrs)
             {:link-target (.toString (Files/readSymbolicLink path))}))))

(defn files [dir]
  (let [dir (if (map? dir)
               (:path dir)
               dir)]
    (->> dir
         as-path
         Files/list
         .iterator
         iterator-seq
         (map attrs))))

(defn tree [root pred]
  (let [root (if (string? root)
               (attrs root)
               root)]
    (tree-seq #(and (:directory? %)
                    pred
                    (pred %))
              files
              root)))

(defn name-key [subj]
  (s/lower-case (:name subj)))

(defn- old-fashioned-sort-key [subj]
  (str (if (:directory? subj) 0 1)
       (name-key subj)))

(defn hidden? [subj]
  (s/starts-with? (:name subj) "."))

(def not-hidden? (complement hidden?))

(defn- regex? [subj] (instance? java.util.regex.Pattern subj))

(defn- pattern? [subj]
  (or (regex? subj)
      (and (string? subj)
           (or (s/includes? subj "*")
               (s/includes? subj "?")))))

(defn- mk-pattern [subj]
  (if (regex? subj)
    subj
    (re-pattern (-> subj
                    (s/replace "." "\\.")
                    (s/replace "*" ".*")
                    (s/replace "?" ".")))))

(defn- filename-matches? [pattern subj]
  (if pattern
    (re-matches pattern (:name subj))
    true))

(defn- mk-matcher [subj]
  (cond
    (fn? subj) subj
    (regex? subj) (fn [file] (filename-matches? subj file))
    (string? subj) (let [pattern (mk-pattern subj)]
                     (fn [file] (filename-matches? pattern file)))
    :else (constantly true)))

(defn- expand-flags [subj flags]
  (->> (sort-by flags subj)
       (mapcat #(get flags % [%]))))

(defn- default-arg [arg rest]
  (if (or (empty? rest)
          (keyword? (first rest)))
    (cons arg rest)
    rest))

(defn l
  "Lists files in a directory, args can be:
  string - in this case it is treated as a directory to list files in (with cwd as a base directory),
  regex - used to filter files,
  pattern - a string with wildcards * or ? used to filter files,
  flags - one of:
   :m - to show name, size and modification timestamp,
   :l - long format to show permissions, user, group, size, timestamp and a name,
   :t - to sort by modification timestamp ascending,
   :T - to sort by modification timestamp descending,
   :s - to sort by size ascending,
   :S - to sort by size descending,
   :n - to sort by name ascending,
   :N - to sort by name descending,
   :h - to show hidden files
  "
  [& args]
  (let [flags {:m [:cols [:size :modified :name-ex]]
               :l [:cols [:permissions :user :group :size :modified :name-ex]]
               :t [:sort :modified]
               :T [:sort [:modified :rev]]
               :s [:sort :size]
               :S [:sort [:size :rev]]
               :n [:sort name-key]
               :N [:sort [name-key :rev]]
               :h [:filter (constantly true)]}
        [arg & {:keys [cols sort filter] :or
                 {cols [:name]
                  filter not-hidden?
                  sort old-fashioned-sort-key}}] (default-arg "." (expand-flags args flags))
        [f filter2] (if (or (fn? arg) (pattern? arg))
                      ["." (mk-matcher arg)]
                      [arg (constantly true)])
        path (resolve-path *cwd* f)
        [keyfn cmp] (if (vector? sort)
                      [(first sort) #(- (compare %1 %2))]
                      [sort compare])]
    (with-meta (if (or filter2
                       (:directory? (attrs path)))
                 (->> (files path)
                      (clojure.core/filter filter)
                      (clojure.core/filter filter2)
                      (sort-by keyfn cmp))
                 [(attrs path)])
      {:rackushka/hint [:table :rackushka.fs/file cols]})))

(defn f
  "Find files, args can be:
  string - a directory to search files in,
  regex - a regular expression to filter files,
  pattern - a string with wildcards * or ? used to filter files,
  function - a function to filter files
  :skip regex|pattern|function - a condition to exclude files
  "
  [& args]
  (let [arg1 (first args)
        args (if (and (string? arg1)
                      (not (pattern? arg1)))
               args
               (cons "." args))
        [path & rest-args] args
        abs-path (resolve-path *cwd* path)
        exprs (take-while #(not (keyword? %)) rest-args)
        [& {skip :skip}] (drop-while #(not (keyword? %)) rest-args)
        skip-fn (if skip
                  (complement (mk-matcher skip))
                  (constantly true))
        filters (cons skip-fn
                      (map mk-matcher exprs))]
    (with-meta 
      (->> (rest (tree abs-path skip-fn))
           (filter (apply every-pred filters))
           (map #(assoc % :name (relative-path abs-path (:path %)))))
      {:rackushka/hint [:table :rackushka.fs/file [:name]]})))

(defmulti view-text-file {:private true} :subtype)

(defmethod view-text-file :default [{url :url}]
  (with-meta (data/read-lines url) {:rackushka/hint :text}))

(defmethod view-text-file :xml [{url :url}]
  (data/read-xml url))

(defmethod view-text-file :csv [{url :url}]
  (data/read-csv url))

(defmulti view-app-file {:private true} :subtype)

(defmethod view-app-file :json [{url :url}]
  (data/read-json url))

(defmethod view-app-file :xml [{url :url}]
  (data/read-xml url))

(defmulti view-file :type)

(defmethod view-file :text [subj]
  (view-text-file subj))

(defmethod view-file :application [subj]
  (view-app-file subj))

(defmethod view-file :image [{url :url}]
  (with-meta
    [:img {:src url}]
    {:rackushka/hint :html}))

(defn- add-trailing-slash [subj]
  (if (= (last subj) \/)
    subj
    (str subj "/")))

(defn- absolute-url [subj]
  (str 
   (java.net.URL.
    (java.net.URL. (str "file://" (add-trailing-slash *cwd*)))
    subj)))

(defn- mime-type [url]
  (when-let [ct (.detect (Tika.) url)]
    (map keyword (-> ct
                     (s/split #";")
                     first
                     (s/split #"/" 2)))))

(defn v
  "View file content."
  [url]
  (let [url (absolute-url url)]
    (when-let [[type subtype] (mime-type (java.net.URL. url))]
      (view-file {:url url :type type :subtype subtype}))))


(defn t
  "Detect file mime type"
  [subj]
  (let [url (if (map? subj)
                   (:path subj)
                   (str subj))]
    (mime-type (java.net.URL. (absolute-url url)))))

(defn match-mime-type
  ([tp] (fn [subj] (= (first (t subj)) tp)))
  ([tp stp] (fn [subj] (= (t subj) [tp stp]))))
