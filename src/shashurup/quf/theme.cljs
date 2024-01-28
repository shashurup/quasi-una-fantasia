(ns shashurup.quf.theme)

(def colors #{:bg :sel-bg :alt-bg :fg
              :symbol :literal :string :keyword
              :error})

(defn- var-name [color-key]
  (str "--color-" (name color-key)))

(defn computed-style []
  (.getComputedStyle js/window (.-documentElement js/document)))

(defn var-value [var-name]
  (.getPropertyValue (computed-style) var-name))

(defn get-theme []
  (into {} (map #(vector % (var-value (var-name %))) colors)))

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
  {:base00 :bg
   :base02 :sel-bg
   :base05 :fg
   :base07 :alt-bg
   :base08 :symbol
   :base09 :literal
   :base0B :string
   :base0D :keyword
   :base0F :error})
