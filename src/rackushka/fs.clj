(ns rackushka.fs
  (:import [java.nio.file Files Paths LinkOption]
           [java.nio.file.attribute PosixFilePermission]))

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

(defn- list-by-path [path]
  (->> path
       Files/list
       .iterator
       iterator-seq))

(defn list [path]
  (map file-by-path
       (list-by-path (make-path path))))

(defn ls [path]
  (with-meta (list path)
    {:rackushka/hint [:table :rackushka.fs/file
                      [:size :modified :permissions :name-ex]]}))
