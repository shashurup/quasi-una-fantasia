(ns rackushka.naive-parser)

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
  (let [{layout :layout} (reduce handle-char
                                 {:in :whitespace
                                  :start 0
                                  :layout []}
                                 (map-indexed vector (str subj \u0000)))]
    (map (fn [[start end in]]
           [(subs subj start end) in]) layout)))
