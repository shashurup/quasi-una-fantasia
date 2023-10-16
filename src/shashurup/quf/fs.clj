(ns shashurup.quf.fs
  (:import [java.nio.file Files Paths LinkOption CopyOption]
           [java.nio.file.attribute PosixFilePermission FileAttribute]
           [java.time Instant]
           [java.time.temporal ChronoUnit]
           [org.apache.tika Tika])
  (:require [clojure.string :as s]
            [shashurup.quf.quf :as quf]
            [shashurup.quf.data :as data]
            [shashurup.quf.events :as events]))

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

(defn- exists? [path]
  (Files/exists (as-path path) no-opts))

(defn- path-file-name [path]
  (.getFileName (as-path path)))

(defn- expand-tilde [subj]
  (if (s/starts-with? subj "~")
    (str (System/getenv "HOME") (subs subj 1))
    subj))

(defn- resolve-path [base path]
  (str (.normalize (.resolve (as-path base)
                             (as-path (expand-tilde path))))))

(defn relative-path [path other]
  (str (.normalize (.relativize (as-path path)
                                (as-path other)))))

(def ^:dynamic *cwd*)

(defn c
  "Change current directory."
  [path]
  (def ^:dynamic *cwd* (resolve-path *cwd* path))
  *cwd*)

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
            :name (str (.getFileName path))
            :mime-type (Files/probeContentType path)}
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
               :c [:cols [:content :name-ex] :mode :list]
               :t [:sort :modified]
               :T [:sort [:modified :rev]]
               :s [:sort :size]
               :S [:sort [:size :rev]]
               :n [:sort name-key]
               :N [:sort [name-key :rev]]
               :h [:filter (constantly true)]}
        [arg & {:keys [cols sort filter mode] :or
                {mode :table
                 cols [:name]
                 filter not-hidden?
                 sort old-fashioned-sort-key}}] (default-arg "." (expand-flags args flags))
        [f filter2] (if (or (fn? arg) (pattern? arg))
                      ["." (mk-matcher arg)]
                      [arg (constantly true)])
        path (resolve-path *cwd* f)
        [keyfn cmp] (if (vector? sort)
                      [(first sort) #(- (compare %1 %2))]
                      [sort compare])]
    (quf/hint (if (or filter2
                      (:directory? (attrs path)))
                (->> (files path)
                     (clojure.core/filter filter)
                     (clojure.core/filter filter2)
                     (sort-by keyfn cmp))
                [(attrs path)])
              [mode :shashurup.quf.fs/file cols])))

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
    (quf/hint
     (->> (rest (tree abs-path skip-fn))
          (filter (apply every-pred filters))
          (map #(assoc % :name (relative-path abs-path (:path %)))))
     [:table :shashurup.quf.fs/file [:name]])))

(defn m-mtype [pattern]
  (fn [{mt :mime-type}]
    (when mt
      (re-matches pattern mt))))

(def time-units {:second 1000
                 :minute (* 60 1000)
                 :hour (* 60 60 1000)
                 :day (* 24 60 60 1000)
                 :week (* 7 24 60 60 1000)
                 :month (* 30 24 60 60 1000)
                 :year (* 365 24 60 60 1000)
                 :today ChronoUnit/DAYS
                 ;; this doesn't work :(
                 ;; :this-week ChronoUnit/WEEKS
                 ;; :this-month ChronoUnit/MONTHS
                 ;; :this-year ChronoUnit/YEARS
                 })

(defn m-modified-in
  ([term]
   (fn [{mod :modified}]
     (> mod (.toEpochMilli (.truncatedTo (Instant/now)
                                         (get time-units term ChronoUnit/DAYS))))))
  ([amount unit]
   (fn [{mod :modified}]
     (> mod (- (System/currentTimeMillis)
               (* amount (get time-units unit :day)))))))

(defn- add-trailing-slash [subj]
  (if (= (last subj) \/)
    subj
    (str subj "/")))

(defn- absolute-url [subj]
  (java.net.URL.
   (java.net.URL. (str "file://" (add-trailing-slash *cwd*)))
   (expand-tilde subj)))

(defn- get-file-url [subj]
  (absolute-url (if (map? subj)
                  (:path subj)
                  subj)))

(defn- naive-mime-type [url]
  (Files/probeContentType (as-path (.getPath url))))

(defn- deep-mime-type [url]
  (.detect (Tika.) url))

(defn get-file-mime-type [url]
  (or (naive-mime-type url)
      (deep-mime-type url)))

(defn v
  "View file content."
  [subj]
  (let [obj (if (map? subj) subj {:path subj})
        obj (update obj :mime-type (fn [mt]
                                     (if mt
                                       mt
                                       (let [url (absolute-url (:path obj))]
                                         (or (naive-mime-type url)
                                             (deep-mime-type url))))))]
    (quf/hint obj [:object-attr :shashurup.quf.fs/file :content])))


(defn t
  "Detect file mime type"
  [subj]
  (deep-mime-type (get-file-url subj)))

(defmulti read-file {:private true} second)

(defmethod read-file "text/csv" [[url _]]
  (data/from-csv url))

(defmethod read-file "application/xml" [[url _]]
  (data/from-xml url))

(defmethod read-file "application/json" [[url _]]
  (data/from-json url))

(defmethod read-file :default [[url mime-type]]
  (if (s/starts-with? mime-type "text/")
    (data/as-text url)
    (slurp url)))

(defn r
  "Read file content."
  [subj]
  (let [url (get-file-url subj)
        mime-type (when (map? subj) (:mime-type subj))]
    (read-file [(str url) (or mime-type (get-file-mime-type url))])))

(defn- mk-target-path [source target-dir]
  (.resolve (as-path target-dir) (path-file-name source)))

(defn- copy-tree [source target]
  (let [source (as-path source)
        target (as-path target)]
    (concat 
     [(str (Files/copy source target (make-array CopyOption 0)))]
     (when (:directory? (attrs source))
       (->> source
            files
            (mapcat (fn [{p :path}]
                      (copy-tree p (mk-target-path p target))))
            doall)))))

(defn- move-tree [source target]
  [(str (Files/move (as-path source)
                    (as-path target)
                    (make-array CopyOption 0)))])

(defn- arg->path [subj]
  (if (map? subj)
    (:path subj)
    (resolve-path *cwd* subj)))

(defn- act-on-files 
  [f & args]
  (let [pathes (map arg->path (flatten args))
        target (last pathes)]
    (cond
      (< (count pathes) 2) (throw (Exception. "At least two args are expected."))
      (= (count pathes) 2) (let [source (first pathes)
                                 target (if (exists? target)
                                          (mk-target-path source target)
                                          target)]
                             (f source target))
      :else (if (not (:directory? (attrs target)))
              (throw (Exception. "The last arg must be a directory."))
              (->> (butlast pathes)
                   (mapcat #(f % (mk-target-path % target))))))))

(defn copy
  "Copy file(s)."
  [& args]
  (apply act-on-files copy-tree args))

(defn move
  "Move file(s)."
  [& args]
  (apply act-on-files move-tree args))

(defn del-tree [subj]
  (conj 
   (when (:directory? (attrs subj))
     (->> (files subj)
          (map :path)
          (mapcat del-tree)
          doall))
   (do (Files/delete (as-path subj))
       (str subj))))

(defn del
  "Delete file(s)."
  [& args]
  (->> (flatten args)
       (map arg->path)
       (mapcat del-tree)
       doall))

(def rm del)

(defn create-dir [subj]
  (str (Files/createDirectory (as-path (arg->path subj))
                              (make-array FileAttribute 0))))

(def mkdir create-dir)

(events/push {:type :require :ns "shashurup.quf.fs"})
