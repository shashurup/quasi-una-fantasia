(ns shashurup.quf.theme
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :refer [includes?]]
            [shashurup.quf.response :as resp]))

(defn- lightness [r g b]
  (/ (+ (max r g b) (min r g b)) 2))

(defn- parse-color [color]
  (->> color
       (partition 2)
       (map #(apply str %))
       (map #(Integer/parseInt % 16))))

(defn- dark? [theme]
  (let [bgcolor (:bg theme)]
    (< (apply lightness (parse-color bgcolor)) 128)))

(def predef {:dark dark?
             :light (complement dark?)})

;; base 16

;; legend
;; base00 - Default Background
;; base01 - Lighter Background (Used for status bars, line number and folding marks)
;; base02 - Selection Background
;; base03 - Comments, Invisibles, Line Highlighting
;; base04 - Dark Foreground (Used for status bars)
;; base05 - Default Foreground, Caret, Delimiters, Operators
;; base06 - Light Foreground (Not often used)
;; base07 - Light Background (Not often used)
;; base08 - Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
;; base09 - Integers, Boolean, Constants, XML Attributes, Markup Link Url
;; base0A - Classes, Markup Bold, Search Text Background
;; base0B - Strings, Inherited Class, Markup Code, Diff Inserted
;; base0C - Support, Regular Expressions, Escape Characters, Markup Quotes
;; base0D - Functions, Methods, Attribute IDs, Headings
;; base0E - Keywords, Storage, Selector, Markup Italic, Diff Changed
;; base0F - Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>

(def base16-map
  {:scheme :name
   :author :author
   :base00 :bg
   :base02 :sel-bg
   :base05 :fg
   :base01 :alt-bg
   :base08 :symbol
   :base09 :literal
   :base0B :string
   :base0D :keyword
   :base0F :error})

(defn from-base16 [subj]
  (update-keys (select-keys subj (keys base16-map)) base16-map))

(defn ls
  ([] (ls identity))
  ([pred]
   (let [base16 (with-open [r (io/reader "resources/public/base16.edn")]
                  (edn/read (java.io.PushbackReader. r)))
         match (cond
                 (fn? pred) pred
                 (string? pred) #(includes? % pred)
                 (keyword? pred) (predef pred (constantly false))
                 :else (constantly false))]
     (resp/hint
      (filter match (map from-base16 base16))
      [:list :shashurup.quf.theme/theme [:preview :name]]))))

(resp/client-require "shashurup.quf.theme")
