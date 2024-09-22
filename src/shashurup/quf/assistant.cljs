(ns shashurup.quf.assistant
  (:require
   [clojure.string :as s]
   [shashurup.quf.editor :as editor]
   [shashurup.quf.history :as history]
   [shashurup.quf.markup :as markup]
   [shashurup.quf.nrepl :as nrepl]
   [crate.core :as crate]
   [goog.dom :as gdom]
   [goog.dom.classes :as gcls]))

(defonce completions-timer (atom nil))

(defn schedule [f delay]
  (let [timer-id (js/setTimeout f delay)]
    (reset! completions-timer timer-id)))

(defn cancel []
  (when-let [timer-id @completions-timer]
    (js/clearTimeout timer-id)))

(defn get-root [id]
  (gdom/getElement (str "assistant-" id)))

(defn get-cand-root [id]
  (gdom/getElement (str "cand-" id)))

(defn get-doc-root [id]
  (gdom/getElement (str "doc-" id)))

(defn get-cell [id]
  (gdom/getElement (str "cell-" id)))

(defn show [id]
  (when-let [root (get-root id)]
    (let [style (.-style root)
          viewport-height (.-clientHeight (.-documentElement js/document))
          height (.-offsetHeight root)
          expr (gdom/getElement (str "expr-" id))
          expr-left-offset (.-offsetLeft expr)
          expr-top-offset (.-offsetTop expr)
          expr-bottom  (.-bottom (.getBoundingClientRect expr))
          expr-top  (.-top (.getBoundingClientRect expr))]
      (. js/console debug
         "vp-height" viewport-height
         "height" height
         "expr-top-offset" expr-top-offset
         "expr-bottom" expr-bottom
         "expr-top" expr-top)
      (set! (.-left style) (str expr-left-offset "px"))
      (if (> (+ expr-bottom height) viewport-height)
        (set! (.-top style) (str (- expr-top-offset height) "px"))
        (set! (.-top style) "auto"))
      (set! (.-visibility style) "visible"))))

(defn hide [id]
  (when-let [root (get-root id)]
    (set! (.-visibility (.-style root)) "hidden")))

(defn update-visibility [id]
  (when-let [root (get-root id)]
    (let [cand (get-cand-root id)
          doc (get-doc-root id)]
      (if (or (.hasChildNodes cand)
              (.hasChildNodes doc))
        (show id)
        (hide id)))))

(defn container-at-point []
  (.-startContainer (.getRangeAt (js/getSelection) 0)))

(defn text-at-point []
  (.-textContent (container-at-point)))

(defn sym-or-kwd-at-point? []
  (let [span (.-parentElement (container-at-point))]
    (or (gcls/has span "quf-symbol")
        (gcls/has span "quf-keyword"))))

(defn words-at-cell-input [id]
  (let [text (.-textContent (gdom/getElement (str "expr-" id)))]
    (filter not-empty (s/split text #"[\s()]"))))

(defn select-candidate [candidate]
  (gcls/add candidate "quf-selected"))

(defn deselect-candidate [candidate]
  (gcls/remove candidate "quf-selected"))

(defn find-selected-candidate [parent]
  (gdom/getElementByClass "quf-selected" parent))

(defn active [id]
  (find-selected-candidate (get-cand-root id)))

(defn move-selection [id move-fn]
  (when-let [parent (get-cand-root id)]
    (when-let [current (find-selected-candidate parent)]
      (when-let [next (move-fn current)]
        (select-candidate next)
        (deselect-candidate current)
        next))))

(defn select-next-candidate [id]
  (move-selection id gdom/getNextElementSibling))

(defn select-prev-candidate [id]
  (move-selection id gdom/getPreviousElementSibling))

(defn clear
  "Hide completion candidates."
  {:keymap/key :hide-completion-candidates}
  [id]
  (let [parent (get-cand-root id)
        doc-root (get-doc-root id)]
    (gcls/set parent "quf-candidates")
    (gdom/removeChildren parent)
    (gdom/removeChildren doc-root)
    (hide id)))

(defn move-cursor-to-the-end-of [target]
  (.setStart (.getRangeAt (js/getSelection) 0)
             target
             (gdom/getNodeTextLength target)))

(defn- assistant-content-class [element]
  (some #{"quf-whole-expr" "quf-at-point"}
        (gcls/get element)))

(defmulti apply-candidate #(assistant-content-class %2))

(defn replace-text-at-point [new-text]
  (when-let [target (container-at-point)]
    (gdom/setTextContent target new-text)
    (move-cursor-to-the-end-of target)))

(defmethod apply-candidate "quf-at-point" [id candidate]
  (replace-text-at-point (gdom/getTextContent candidate)))

(defn rightmost-child [node]
  (if-let [last-child (.-lastChild node)]
    (rightmost-child last-child)
    node))

(defmethod apply-candidate "quf-whole-expr" [id candidate]
  (when-let [input (gdom/getElement (str "expr-" id))]
    (gdom/copyContents input candidate)
    (move-cursor-to-the-end-of (rightmost-child input))))

(defn use-candidate [id]
  (when-let [parent (get-cand-root id)]
    (when-let [selected (find-selected-candidate parent)]
      (apply-candidate id selected)
      (gcls/set parent "quf-candidates")
      (gdom/removeChildren parent))))

(defn use-next-candidate
  "Choose next completion candidate."
  {:keymap/key :use-next-candidate}
  [id]
  (apply-candidate id (select-next-candidate id)))

(defn use-prev-candidate
  "Choose previous completion candidate."
  {:keymap/key :use-prev-candidate}
  [id]
  (apply-candidate id (select-prev-candidate id)))

(defn find-first-matching-candidate [parent substring]
  (->> (gdom/getElementsByTagName "span" parent)
       (filter #(s/includes? (gdom/getTextContent %) substring))
       first))

(defmulti render-candidate #(when (map? %1) (:type %)))

(defmethod render-candidate :default [subj class]
  (crate/html [:span {:class class}
               (editor/structure->html (markup/parse subj))]))

(defmethod render-candidate :function [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :keyword [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :macro [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :namespace [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :var [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :special-form [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :class [subj class] (render-candidate (:candidate subj) class))

(def max-completions 16)

(defn show-candidates [id candidates class]
  (let [tail (if (> (count candidates) max-completions) "..." "")
        target (get-cand-root id)]
    (gdom/removeChildren target)
    (gcls/set target "quf-candidates")
    (when (not-empty candidates)
      (gcls/add target class)
      (doseq [c (take max-completions candidates)]
        (.append target (render-candidate c class))
        (.append target " "))
      (.append target tail)
      (select-candidate (gdom/getFirstElementChild target)))
    (update-visibility id)))

(defn initiate-at-point [id]
  (cancel)
  (if (sym-or-kwd-at-point?)
    (nrepl/send-completions (text-at-point) #(show-candidates id % "quf-at-point"))
    (show-candidates id [] nil)))

(defn initiate-history
  "Initiate a search through the history."
  {:keymap/key :search-history}
  [id]
  (cancel)
  (let [candidates (->> (words-at-cell-input id)
                        history/search
                        (take max-completions))]
    (show-candidates id candidates "quf-whole-expr")))

(defn common-prefix [a b]
  (->> (range (inc (count a)))
       (map #(subs a 0 %))
       (filter #(s/includes? b %))
       last))

(defn longest-prefix [coll]
  (reduce common-prefix coll))

(defn complete-and-show [id candidates class]
  (let [completion (longest-prefix (map :candidate candidates))]
    (replace-text-at-point completion))
  (show-candidates id candidates class))

(defmulti attempt-complete
  "Attempt complete the symbol under cursor."
  {:keymap/key :attempt-complete}
  #(assistant-content-class (get-cand-root %)))

(defn attempt-complete-at-point [id]
  (cancel)
  (when (sym-or-kwd-at-point?)
    (nrepl/send-completions (text-at-point)
                            #(complete-and-show id % "quf-at-point"))))

(defmethod attempt-complete :default [id]
  (attempt-complete-at-point id))

(defmethod attempt-complete "quf-at-point" [id]
  (attempt-complete-at-point id))

(defmethod attempt-complete "quf-whole-expr" [id]
  (use-candidate id))

(defn show-doc [id reply]
  (when-let [doc (:out reply)]
    (let [root (get-doc-root id)]
      (gdom/append root doc)
      (show id))))

(defn toggle-doc
  "Show/hide a documentation for the symbol under cursor."
  {:keymap/key :toggle-doc}
  [id]
  (let [root (get-doc-root id)
        selected (find-selected-candidate (get-cand-root id))
        subj (if selected
               (gdom/getTextContent selected)
               (text-at-point))]
    (if (empty? (gdom/getTextContent root))
      (nrepl/send-eval-aux (str "(clojure.repl/doc " subj ")")
                           #(show-doc id %))
      (do
        (gdom/removeChildren root)
        (update-visibility id)))))

(defmulti handle-input-change #(assistant-content-class (get-cand-root %)))

(defmethod handle-input-change :default [id]
  (schedule #(initiate-at-point id) 1000))

(defmethod handle-input-change "quf-at-point" [id]
  (let [old-selected (active id)]
    (if-let [new-selected (find-first-matching-candidate (get-cand-root id)
                                                         (text-at-point))]
      (do 
        (deselect-candidate old-selected)
        (select-candidate new-selected))
      (initiate-at-point id))))

(defmethod handle-input-change "quf-whole-expr" [id]
  (initiate-history id))

(defn on-input-change [id]
  (cancel)
  (handle-input-change id))

(defn on-focus-out [id]
  (cancel)
  (clear id))

(defn plug [input id]
  (.addEventListener input "focusout" #(on-focus-out id))
  (.addEventListener input "input" #(on-input-change id)))
