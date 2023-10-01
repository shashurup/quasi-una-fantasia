(ns shashurup.quf.fs
  (:require [clojure.string :as s]
            [shashurup.quf.desc :as desc]
            [shashurup.quf.utils :as u]))

(defn- local-class [subj]
  (str "quf-fs-" subj))

(defn- render-name [[name directory? symlink?]]
  (let [class (cond
                directory? "directory"
                symlink?   "symlink"
                :else      "file")]
    [:span {:class (local-class class)} name]))

(defn- render-name-with-link [[name directory? symlink? target] ]
  (let [name-span (render-name [name directory? symlink?])]
    (if symlink?
      (list name-span " -> " [:span {:class (local-class "link-target")} target])
      name-span)))

(def permmap [[:owner-read     \r]
              [:owner-write    \w]
              [:owner-execute  \x]
              [:group-read     \r]
              [:group-write    \w]
              [:group-execute  \x]
              [:others-read    \r]
              [:others-write   \w]
              [:others-execute \x]])

(defn- convert-permissions [subj]
  (apply str (for [[k v] permmap]
               (if (subj k) v \-))))

(defn local-file-url [path]
  (if (s/starts-with? path "/")
    (str "fs" path)
    path))

(defn render-content [[path mime-type]]
  (desc/render-object (local-file-url path) mime-type))

(def file {:columns {:name {:key [:name :directory? :symlink?]
                            :render render-name}
                     :name-ex {:title "Name"
                               :key [:name :directory? :symlink? :link-target]
                               :render render-name-with-link}
                     :size desc/file-size
                     :modified desc/milliseconds
                     :created desc/milliseconds
                     :accessed {:title "Last access"
                                :type :shashurup.quf.desc/milliseconds}
                     :permissions convert-permissions
                     :content {:key [:path :mime-type]
                               :render render-content}}})

(swap! desc/object-types assoc ::file file)

(u/add-style-ref "css/fs.css")
