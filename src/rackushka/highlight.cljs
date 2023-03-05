(ns rackushka.highlight
  (:require
   [clojure.string :as s]
   [crate.core :as crate]
   [goog.dom :as gdom]))

(def class-map {:string "ra-string"
                :number "ra-number"
                :keyword "ra-keyword"
                :symbol "ra-symbol"})

(defn whitespace? [ch]
  (or (#{\[ \] \{ \} \( \) \,} ch)
      (re-matches #"\s" ch)))

; Clojure reqex litral doesn't work for unicode
; properties, we need "u" flag in js
(def alpha-re (js/RegExp. "\\p{Alpha}" "u"))

(defn sym-char? [ch]
  (or (#{\. \* \! \_ \? \$ \% \& \= \< \>} ch)
      (re-matches alpha-re ch)))

(defn num-char? [ch]
  (let [code (.charCodeAt ch 0)]
    (and (>= code 48) (<= code 57))))

(defn handle-char [state [pos ch]]
  (let [{in :in
         hint :hint
         start :start
         layout :layout}  state]
    (merge state
           (if (= ch \u0000)
             (when (> pos 0) {:layout (conj layout [start pos in])})
             (condp = in
               :whitespace (when-let [in (condp = ch
                                           \" {:in :string}
                                           \\ {:in :char}
                                           \: {:in :keyword}
                                           \# {:in :dispatch}
                                           \+ {:in :symbol :hint :sign}
                                           \- {:in :symbol :hint :sign}
                                           (cond 
                                             (num-char? ch) {:in :number}
                                             (sym-char? ch) {:in :symbol}))]
                             (assoc in
                                    :start pos
                                    :layout (if (> pos 0)
                                              (conj layout [start pos :whitespace])
                                              layout)))
               :string (cond
                         (= hint :escape) {:hint nil}
                         (= ch \")        {:in :whitespace
                                           :start (inc pos)
                                           :layout (conj layout [start (inc pos) :string])}
                         (= ch \\)        {:hint :escape})
               :dispatch (if (= ch \")
                           {:in :string :start (dec pos)}
                           {:in :whitespace :start (dec pos)})
               (cond
                 (whitespace? ch) {:in :whitespace
                                   :hint nil
                                   :start pos
                                   :layout (conj layout [start pos in])}
                 (= hint :sign)   (if (num-char? ch)
                                    {:in :number :hint nil}
                                    {:hint nil})))))))

(defn demarkate [subj]
  (:layout (reduce handle-char
                   {:in :whitespace
                    :start 0
                    :layout []}
                   (map-indexed vector (str subj \u0000)))))

(defn layout->html [subj [start stop in]]
  (let [class (class-map in)
        text (subs subj start stop)]
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
  [0
   (count (.-textContent el))
   (condp = (.-nodeType el)
     1 (class->type (.getAttribute el "class"))
     3 :whitespace
     nil)])

(defn existing-markup [el]
  (->> (.-childNodes el)
       (map html->layout)
       (reductions (fn [[start stop in] [_ len type]]
                     [stop (+ stop len) type]))))

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
  (if-let [[num offset _]
           (->> layout
                (map-indexed (fn [idx [start stop in]]
                               [idx
                                (- pos start)
                                (if (= in :whitespace)
                                  (and (>= pos start) (< pos stop))
                                  (and (>= pos start) (<= pos stop)))]))
                (filter #(nth % 2))
                first)]
    [num offset]
    (when-let [[start _ _] (last layout)]
      [(dec (count layout)) (- pos start)])))

(defn highlight [el]
  (let [text (.-textContent el)
        layout (demarkate text)]
    (when (not= (existing-markup el) layout)
      (let [pos (get-cursor-position el)
            [el-num offset] (figure-placement layout pos)]
        ; (.log js/console "different!! " pos text)
        (replace-content el (map #(layout->html text %) layout))
        (set-cursor-position el el-num offset)))))

(defn plug [input]
  (.addEventListener input
                     "input"
                     #(highlight (.-target %))))
