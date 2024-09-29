(ns shashurup.quf.fs
  (:import [java.nio.file Files Paths LinkOption CopyOption]
           [java.nio.file.attribute PosixFilePermission FileAttribute]
           [java.time Instant]
           [java.time.temporal ChronoUnit]
           [org.apache.tika Tika])
  (:require [clojure.string :as s]
            [shashurup.quf.response :as resp]
            [shashurup.quf.data :as data]
            [shashurup.quf.response :as response]))

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

(defn- build-regex [subj]
  (if (regex? subj)
    subj
    (re-pattern (-> subj
                    (s/replace "." "\\.")
                    (s/replace "*" ".*")
                    (s/replace "?" ".")))))

(defn fit?
  ([subj pattern] (fit? subj :name pattern))
  ([subj field pattern]
   (let [regex (build-regex pattern)]
     (re-matches regex (field subj)))))

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

(defn modified-in
  ([{mod :modified} term]
   (> mod (.toEpochMilli (.truncatedTo (Instant/now)
                                       (get time-units term ChronoUnit/DAYS)))))
  ([{mod :modified} amount unit]
   (> mod (- (System/currentTimeMillis)
             (* amount (get time-units unit :day))))))

(defn- filter? [subj]
  (or (fn? subj)
      (keyword? subj)
      (vector? subj)
      (pattern? subj)))

(defn- file-arg? [subj]
  (and (string? subj) (not (filter? subj))))

(defn- first-of [pred coll default]
  (or (first (filter pred coll)) default))

(defn- build-filter [subj]
  (cond
    (fn? subj) subj
    (keyword? subj) subj
    (vector? subj) (apply some-fn (map build-filter subj))
    (regex? subj) #(fit? % subj)
    (string? subj) #(fit? % subj)
    :else any?))

(defn- extract-flags [args singles paired]
  (subvec (reduce (fn [[args flags cur] arg]
                    (cond
                      cur [args (assoc flags cur arg) nil]
                      (singles arg) [args (assoc flags arg true) nil]
                      (paired arg) [args flags arg]
                      :else [(conj args arg) flags nil]))
                  [[] {} nil]
                  args) 0 2))

(defn fmt
  "Formats file list as a table or thumb list.
   flags - one of:
   :1 - to show just a file name
   :m - to show name, size and modification timestamp,
   :l - long format to show permissions, user, group, size, timestamp and a name,
   :c - to show files as a list of thumbnails"
  ([subj] (fmt :1 subj))
  ([flag subj]
   (let [mode (if (= flag :c) :list  :table)
         cols (get {:m [:size :modified :name-ex]
                    :l [:permissions :user :group :size :modified :name-ex]
                    :c [:content :name-ex]}
                   flag [:name])]
     (resp/hint subj [mode :shashurup.quf.fs/file cols]))))

(defn ord
  "Sorts file list.
   :t - by modification timestamp ascending,
   :T - by modification timestamp descending,
   :s - by size ascending,
   :S - by size descending,
   :n - by name ascending,
   :N - by name descending"
  ([flag subj]
   (let [[key cmp] (get {:t [:modified compare]
                         :T [:modified #(- (compare %1 %2))]
                         :s [:size compare]
                         :S [:size #(- (compare %1 %2))]
                         :n [:name compare]
                         :N [:name #(- (compare %1 %2))]}
                        flag
                        [old-fashioned-sort-key compare])]
     (sort-by key cmp subj)))
  ([subj] (ord :dummy subj)))

(defn l
  "Lists files in a directory, args can be:
  string - in this case it is treated as a directory to list files in (with cwd as a base directory),
  regex - used to filter files,
  pattern - a string with wildcards * or ? used to filter files,
  vector - list of regexes or patterns to match
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
   :c - to show files as a list of thumbnails
  "
  [& args]
  (let [[args flags] (extract-flags args #{:m :l :c :t :T :s :S :n :N :h} #{})
        path (resolve-path *cwd* (first-of file-arg? args "."))
        fmt-flag (some #{:m :l :c} (keys flags))
        ord-flag (some #{:t :T :s :S :n :N} (keys flags))
        filter1 (if (:h flags) any? not-hidden?)
        filter2 (build-filter (first-of filter? args any?))
        file-attrs (attrs path)]
    (if (:directory? file-attrs)
      (->> (files path)
           (filter filter1)
           (filter filter2)
           (ord ord-flag)
           (fmt fmt-flag))
      (fmt fmt-flag [file-attrs]))))

(defn f
  "Find files, args can be:
  string - a directory to search files in,
  regex - a regular expression to filter files,
  pattern - a string with wildcards * or ? used to filter files,
  vector - list of regexes or patterns to match
  function - a function to filter files
  :skip regex|pattern|function - a condition to exclude files
  flags - one of:
   :m - to show name, size and modification timestamp,
   :l - long format to show permissions, user, group, size, timestamp and a name,
   :c - to show files as a list of thumbnails
   :t - to sort by modification timestamp ascending,
   :T - to sort by modification timestamp descending,
   :s - to sort by size ascending,
   :S - to sort by size descending,
   :n - to sort by name ascending,
   :N - to sort by name descending,
  "
  [& args]
  (let [[args flags] (extract-flags args #{:m :l :c :t :T :s :S :n :N} #{:skip})
        path (resolve-path *cwd* (first-of file-arg? args "."))
        fmt-flag (some #{:m :l :c} (keys flags))
        ord-flag (some #{:t :T :s :S :n :N} (keys flags))
        filter1 (complement (build-filter (:skip flags (constantly false))))
        filter2 (build-filter (first-of filter? args any?))
        ]
    (->> (tree path filter1)
         (filter filter1)
         (filter filter2)
         (map #(assoc % :name (relative-path path (:path %))))
         (ord ord-flag)
         (fmt fmt-flag))))

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
  (let [obj (if (map? subj) subj {:path (resolve-path *cwd* subj)})
        obj (update obj :mime-type (fn [mt]
                                     (if mt
                                       mt
                                       (let [url (absolute-url (:path obj))]
                                         (or (naive-mime-type url)
                                             (deep-mime-type url))))))]
    (resp/hint obj [:object-attr :shashurup.quf.fs/file :content])))


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

(defonce _dummy (c "~"))

(resp/client-module :fs)
