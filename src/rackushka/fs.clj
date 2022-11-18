(ns rackushka.fs
  (:import [java.nio.file Files Paths LinkOption]
           [java.nio.file.attribute PosixFilePermission])
  (:require [clojure.string :as s]))

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

(defn- make-path [subj]
  (Paths/get subj (into-array java.lang.String [])))

(defonce cwd (atom "/"))

(defn- resolve-path [base path]
  (str (.normalize (.resolve (make-path base)
                             (make-path path)))))

(defn cd [path]
  (swap! cwd #(resolve-path % path)))

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

(defn- file-by-path [path]
  (let [attrs (Files/readAttributes path
                                    "posix:*"
                                    (into-array [LinkOption/NOFOLLOW_LINKS]))]
    (merge (convert-attrs attrs)
           {:path (.toString path)
            :name (.toString (.getFileName path))}
           (when (symlink? attrs)
             {:link-target (.toString (Files/readSymbolicLink path))}))))

(defn file [path]
  (file-by-path (make-path path)))

(defn- files-by-path [path]
  (->> path
       Files/list
       .iterator
       iterator-seq))

(defn files [path]
  (map file-by-path
       (files-by-path (make-path path))))

(defn name-key [subj]
  (s/lower-case (:name subj)))

(defn old-fashioned-sort-key [subj]
  (str (if (:directory? subj) 0 1)
       (name-key subj)))

(defn hidden? [subj]
  (s/starts-with? (:name subj) "."))

(defn regex? [subj] (instance? java.util.regex.Pattern subj))

(defn pattern? [subj]
  (or (regex? subj)
      (and (string? subj)
           (or (s/includes? subj "*")
               (s/includes? subj "?")))))

(defn mk-pattern [subj]
  (if (regex? subj)
    subj
    (re-pattern (-> subj
                    (s/replace "*" ".*")
                    (s/replace "?" ".")))))

(defn filename-matches? [pattern subj]
  (if pattern
    (re-matches pattern (:name subj))
    true))

(defn expand-flags [subj flags]
  (->> (sort-by flags subj)
       (mapcat #(get flags % [%]))))

(defn ls [& args]
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
                  filter (complement hidden?)
                  sort old-fashioned-sort-key}}] (expand-flags args flags)
        [f pattern] (if (pattern? arg)
                      ["." (mk-pattern arg)]
                        [arg nil])
        path (resolve-path @cwd f)
        [keyfn cmp] (if (vector? sort)
                      [(first sort) #(- (compare %1 %2))]
                      [sort compare])]
    (with-meta (if (or pattern
                       (:directory? (file path)))
                 (->> (files path)
                      (clojure.core/filter filter)
                      (clojure.core/filter #(filename-matches? pattern %))
                      (sort-by keyfn cmp))
                 [(file path)])
      {:rackushka/hint [:table :rackushka.fs/file cols]})))
