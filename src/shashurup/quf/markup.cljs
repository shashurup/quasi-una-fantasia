(ns shashurup.quf.markup
  (:require [clojure.string :as s]
            [clojure.zip :as z]))

(defn non-word? [ch]
  (or (#{\[ \] \{ \} \( \) \, \;} ch)
      (re-matches #"\s" ch)))

(defn word? [ch]
  (not (non-word? ch)))

(defn num-char? [ch]
  (let [code (.charCodeAt ch 0)]
    (and (>= code 48) (<= code 57))))

(defn number-word? [word]
  (or (s/starts-with? word "-")
      (num-char? word)))

(defn handle-char [state [pos ch]]
  (let [{in :in
         hint :hint
         start :start
         layout :layout}  state]
    (merge state
           (if (= ch \u0000)
             (when (> pos 0) {:layout (conj layout [start pos in])})
             (condp = in
               :other (when-let [in (condp = ch
                                           \" {:in :string}
                                           \# {:in :dispatch}
                                           \; {:in :comment}
                                           (when (word? ch)
                                             {:in :word}))]
                             (assoc in :start pos
                                       :layout (conj layout [start pos :other])))
               :string (cond
                         (= hint :escape) {:hint nil}
                         (= ch \")        {:in :other
                                           :start (inc pos)
                                           :layout (conj layout [start (inc pos) :string])}
                         (= ch \\)        {:hint :escape})
               :dispatch (if (= ch \")
                           {:in :string :start (dec pos)}
                           {:in :other :start (dec pos)})
               :comment (when (= ch \newline) {:in :other
                                               :start pos
                                               :layout (conj layout [start pos :comment])})
               (when (non-word? ch) {:in :other
                                     :hint nil
                                     :start pos
                                     :layout (conj layout [start pos in])}))))))

(def opening-parens #{\( \[ \{})

(def closing-parens #{\) \] \}})

(defn classify-character [n v]
  (cond
    (opening-parens v) [n v :open]  ;; unique value for all opening parens
    (closing-parens v) [(* n 2) v :close] ;; unique value for all closing parens
    (= \# v) [(inc n) v :whitespace] ;; the value similar to the next opening parens if any
    :else [-1 v :whitespace]))

(defn combine-characters [subj]
  [(apply str (map second subj))
   (last (last subj))])

(defn handle-item [[value type]]
  (condp = type
    :word (cond
            (s/starts-with? value ":")  [[value :keyword]]
            (s/starts-with? value "\\") [[value :char]]
            (number-word? value)        [[value :number]]
            (#{"true" "false"} value)   [[value :bool]]
            (= "nil" value)             [[value :nil]]
            :else                       [[value :symbol]])
    :other (->> value
                (map-indexed classify-character)
                (partition-by first)
                (map combine-characters))
    [[value type]]))

(defn build-tree [subj]
  (z/root (reduce (fn [tree leaf]
                    (condp = (second leaf)
                      :open (-> tree
                                (z/append-child [[] :container])
                                z/down
                                z/rightmost
                                z/down
                                (z/append-child leaf))
                      :close (-> tree
                                 (z/append-child leaf)
                                 z/up
                                 z/up)
                      (z/append-child tree leaf)))
                  (z/vector-zip [])
                  subj)))

(defn parse [subj]
  (let [{layout :layout} (reduce handle-char
                                 {:in :other
                                  :start 0
                                  :layout []}
                                 (map-indexed vector (str subj \u0000)))]
    (->> layout
         (filter (fn [[start end _]]
                   (> end start)))
         (map (fn [[start end in]]
                [(subs subj start end) in]))
         (map handle-item)
         (apply concat)
         build-tree)))

(defn top-level-forms [subj]
  (->> (parse subj)
       (remove #(= :whitespace (second %)))
       (map #(->> (flatten %)
                  (remove keyword?)
                  (apply str)))))
