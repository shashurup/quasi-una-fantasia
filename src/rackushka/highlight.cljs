(ns rackushka.highlight
  (:require
   [rackushka.naive-parser :as np]
   [clojure.string :as s]
   [crate.core :as crate]
   [goog.dom :as gdom]))

(def class-map {:string "ra-string"
                :number "ra-number"
                :keyword "ra-keyword"
                :symbol "ra-symbol"})

(defn layout->html [[text in]]
  (let [class (class-map in)]
    (if class
      (crate/html [:span {:class class} text])
      text)))

(defn replace-content [el children]
  ; TODO check memfn to replace everything in one step
  (.replaceChildren el)
  (doseq [ch children]
    (.append el ch)))

(defn class->type [subj]
  (keyword (second (s/split subj #"-"))))

(defn html->layout [el]
  [(.-textContent el)
   (condp = (.-nodeType el)
     1 (class->type (.getAttribute el "class"))
     3 :whitespace
     nil)])

(defn existing-markup [el]
  (map html->layout (.-childNodes el)))

(defn text-node-seq [subj]
  (->> subj
       (tree-seq #(.hasChildNodes %)
                 #(.-childNodes %))
       (filter #(= (.-nodeType %) 3))))

(defn get-node-text-offset [node parent]
  (->> parent
       text-node-seq
       (take-while #(not (identical? % node)))
       (map #(.-length %))
       (reduce +)))

(defn get-cursor-position [el]
  (let [range (.getRangeAt (js/getSelection) 0)
        start-el (.-startContainer range)
        start-pos (.-startOffset range)]
    (+ (get-node-text-offset start-el el) start-pos)))

(defn set-cursor-position [el child-num offset]
  (when child-num
    (let [node (nth (text-node-seq el) child-num)]
      (doto (js/getSelection)
        (.removeAllRanges)
        (.addRange (doto (.createRange js/document)
                     (.setStart node offset)
                     (.collapse true)))))))

(defn figure-placement [layout pos]
  (->> layout
       (reductions (fn [[start end type] [text type]]
                     [end (+ end (count text)) type]) [0 0 nil])
       rest
       ;; partitions ovelap so we know which element is the last
       (partition 2 1 [nil])
       (map-indexed (fn [idx [[start end type] next]]
                      [idx
                       (- pos start)
                       ;; when a cursor is after last letter
                       ;; position it into the letter container
                       ;; the same goes for the last whitespace
                       (if (or (not= type :whitespace)
                               (nil? next)) (inc end) end)
                       type]))
       (filter (fn [[_ _ end _]] (< pos end)))
       first
       (take 2)
       ))

(defn highlight [el]
  (let [text (.-textContent el)
        layout (np/demarkate text)]
    (when (not= (existing-markup el) layout)
      (let [pos (get-cursor-position el)
            [el-num offset] (figure-placement layout pos)]
        ;; (.log js/console "different!! " pos text)
        (replace-content el (map layout->html layout))
        (set-cursor-position el el-num offset)))))

(defn plug [input]
  (highlight input)
  (.addEventListener input
                     "input"
                     #(highlight (.-target %))))
