(ns rackushka.fs
  (:require [rackushka.desc :as desc]))

(defn- local-class [subj]
  (str "rackushka-fs-" subj))

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

(def file {:columns {:name {:key [:name :directory? :symlink?]
                            :render render-name}
                     :name-ex {:title "Name"
                               :key [:name :directory? :symlink? :link-target]
                               :render render-name-with-link}
                     :size :rackushka.desc/file-size
                     :modified :rackushka.desc/millisecons
                     :created :rackushka.desc/millisecons
                     :accessed {:title "Last access"
                                :type :rackushka.desc/millisecons}
                     :permissions convert-permissions}})

(swap! desc/object-types assoc ::file file)
