(ns shashurup.quf.theme
  (:require [clojure.string :as s]
            [cljs.tools.reader :refer [read-string]]
            [shashurup.quf.desc :as desc]
            [shashurup.quf.editor :as editor]
            [shashurup.quf.markup :as markup]
            [shashurup.quf.utils :as u]))

(u/begin-module-load! :theme)

(def colors [:bg :sel-bg :alt-bg :fg
             :symbol :literal :string :keyword
             :error])

(defn- var-name [color-key]
  (str "--color-" (name color-key)))

(defn computed-style []
  (.getComputedStyle js/window (.-documentElement js/document)))

(defn var-value [var-name]
  (.getPropertyValue (computed-style) var-name))

(defn get-theme []
  (into {} (map #(vector % (var-value (var-name %))) colors)))

(defn local-storage []
  (.-localStorage js/window))

(defn set-theme [theme]
  (doseq [[k v] theme]
    (.setProperty (.-style (.-documentElement js/document))
                  (var-name k)
                  v))
  (.setItem (local-storage)
            "quf-theme"
            theme))

(defn init []
  (when-let [item (.getItem (local-storage) "quf-theme")]
    (set-theme (read-string item))))

(def preview-text "
(defn function1 [data]
  {:name \"Just a string\"
   :value 42
   :valid false})
")

(defn update-attrs [subj attrs]
  (if (coll? subj)
    (if (map? (second subj))
      (update subj 1 #(merge % attrs))
      (vec (concat [(first subj)] [attrs] (rest subj))))
    subj))

(defn make-theme [theme-colors]
  (into {} (map-indexed #(vector (nth colors %1)
                                 (str "#" %2)) theme-colors)))

(defn render-theme [theme]
  (s/join "\n"
          (concat
           (map (fn [[v c]]
                  (str (var-name v) ": " c ";")) theme)
           [(str "background-color: " (:bg theme) ";")
            (str "color: " (:fg theme) ";")])))

(defn render-preview [theme-colors]
  (let [theme (make-theme theme-colors)
        style (str "white-space: break-spaces;
                    cursor: pointer;
                    padding: 2em;"
                   (render-theme theme))]
    [:div {:style style
           :onclick (u/gen-js-call #(set-theme theme))}
     (->> preview-text
          s/trim
          markup/parse
          editor/structure->html)]))

(def theme {:columns {:preview {:key (vec colors)
                                :render render-preview}}})

(swap! desc/object-types assoc ::theme theme)

(u/set-module-loaded!)
