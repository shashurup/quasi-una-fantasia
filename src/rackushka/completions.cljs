(ns rackushka.completions
  (:require
   [clojure.string :as s]
   [rackushka.naive-parser :as np]
   [rackushka.highlight :as hl]
   [rackushka.nrepl :as nrepl]
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

(defn get-root-element [id]
  (gdom/getElement (str "cand-" id)))

(defn get-doc-root [id]
  (gdom/getElement (str "doc-" id)))

(defn container-at-cursor []
  (.-startContainer (.getRangeAt (js/getSelection) 0)))

(defn text-at-cursor []
  (.-textContent (container-at-cursor)))

(defn words-at-cell-input [id]
  (let [text (.-textContent (gdom/getElement (str "expr-" id)))]
    (filter not-empty (s/split text #"[\s()]"))))

(defn select-candidate [candidate]
  (gcls/add candidate "ra-selected"))

(defn deselect-candidate [candidate]
  (gcls/remove candidate "ra-selected"))

(defn find-selected-candidate [parent]
  (gdom/getElementByClass "ra-selected" parent))

(defn active [id]
  (find-selected-candidate (get-root-element id)))

(defn move-selection [id move-fn]
  (when-let [parent (get-root-element id)]
    (when-let [current (find-selected-candidate parent)]
      (when-let [next (move-fn current)]
        (select-candidate next)
        (deselect-candidate current)))))

(defn select-next-candidate [id]
  (move-selection id gdom/getNextElementSibling))

(defn select-prev-candidate [id]
  (move-selection id gdom/getPreviousElementSibling))

(defn clear-candidates [id]
  (let [parent (get-root-element id)
        doc-root (get-doc-root id)]
    (gcls/set parent "ra-candidates")
    (gdom/removeChildren parent)
    (gdom/removeChildren doc-root)))

(defn move-cursor-to-the-end-of [target]
  (.setStart (.getRangeAt (js/getSelection) 0)
             target
             (gdom/getNodeTextLength target)))

(defmulti apply-candidate #(some #{"ra-at-point" "ra-history"}
                                 (gcls/get %2)))

(defn replace-current-container-text [new-text]
  (when-let [target (container-at-cursor)]
    (gdom/setTextContent target new-text)
    (move-cursor-to-the-end-of target)))

(defmethod apply-candidate "ra-at-point" [id candidate]
  (replace-current-container-text (gdom/getTextContent candidate)))

(defmethod apply-candidate "ra-history" [id candidate]
  (when-let [input (gdom/getElement (str "expr-" id))]
    (gdom/copyContents input candidate)
    (move-cursor-to-the-end-of (.-lastChild input))))

(defn use-candidate [id]
  (when-let [parent (get-root-element id)]
    (when-let [selected (find-selected-candidate parent)]
      (apply-candidate id selected)
      (gcls/set parent "ra-candidates")
      (gdom/removeChildren parent))))

(defn find-first-matching-candidate [parent substring]
  (->> (gdom/getElementsByTagName "span" parent)
       (filter #(s/includes? (gdom/getTextContent %) substring))
       first))

(defmulti render-candidate #(when (map? %1) (:type %)))

(defmethod render-candidate :default [subj class]
  (crate/html [:span {:class class}
               (map hl/layout->html
                    (np/demarkate subj))]))

(defmethod render-candidate :function [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :keyword [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :macro [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :namespace [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :var [subj class] (render-candidate (:candidate subj) class))

(defmethod render-candidate :special-form [subj class] (render-candidate (:candidate subj) class))

(def max-completions 16)

(defn show [id candidates class]
 (let [tail (if (> (count candidates) max-completions) "..." "")
       target (get-root-element id)]
   (gdom/removeChildren target)
   (gdom/removeChildren (get-doc-root id))
   (gcls/set target "ra-candidates")
   (when (not-empty candidates)
     (gcls/add target class)
     (doseq [c (take max-completions candidates)]
       (.append target (render-candidate c class))
       (.append target " "))
     (.append target tail)
     (.scrollIntoView (gdom/getElement (str "cell-" id)))
     (select-candidate (gdom/getFirstElementChild target)))))

(defn common-prefix [a b]
  (->> (range (inc (count a)))
       (map #(subs a 0 %))
       (filter #(s/includes? b %))
       last))

(defn longest-prefix [coll]
  (reduce common-prefix coll))

(defn complete-and-show [id candidates class]
  (let [completion (longest-prefix (map :candidate candidates))]
    (replace-current-container-text completion))
  (show id candidates class))

(defn initiate-at-point [id]
  (cancel)
  (nrepl/send-completions (text-at-cursor) #(show id % "ra-at-point")))

(defn attempt-complete [id]
  (cancel)
  (nrepl/send-completions (text-at-cursor)
                          #(complete-and-show id % "ra-at-point")))

(defn initiate-history [id]
  (cancel)
  (nrepl/send-history (words-at-cell-input id)
                      (inc max-completions)
                      #(show id % "ra-history")))

(defn show-doc [id doc]
  (let [root (get-doc-root id)
        doc (->> (:out doc)
                 (map :out)
                 rest
                 s/join)]
    (gdom/setTextContent root doc)
    (.scrollIntoView root)))

(defn toggle-doc [id]
  (let [root (get-doc-root id)
        selected (find-selected-candidate (get-root-element id))
        subj (if selected
               (gdom/getTextContent selected)
               (text-at-cursor))]
    (if (or selected (empty? (gdom/getTextContent root)))
      (nrepl/send-eval (str "(clojure.repl/doc " subj ")")
                       #(show-doc id %))
      (gdom/removeChildren root))))

(defmulti handle-input-change #(some #{"ra-history" "ra-at-point"}
                                     (gcls/get (get-root-element %))))

(defmethod handle-input-change :default [id]
  (schedule #(initiate-at-point id) 1000))

(defmethod handle-input-change "ra-at-point" [id]
  (let [old-selected (active id)]
    (if-let [new-selected (find-first-matching-candidate (get-root-element id)
                                                         (text-at-cursor))]
      (do 
        (deselect-candidate old-selected)
        (select-candidate new-selected))
      (initiate-at-point id))))

(defmethod handle-input-change "ra-history" [id]
  (initiate-history id))

(defn on-input-change [id]
  (cancel)
  (handle-input-change id))

(defn plug [input id]
  (.addEventListener input "focusout" cancel)
  (.addEventListener input "input" #(on-input-change id)))
