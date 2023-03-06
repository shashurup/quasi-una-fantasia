(ns rackushka.completions
  (:require
   [rackushka.nrepl :as nrepl]
   [crate.core :as crate]
   [goog.dom :as gdom]
   [goog.dom.classes :as gcls]))

(defonce completions-timer (atom nil))

(defn schedule [f delay]
  (when-let [timer-id @completions-timer]
    (js/clearTimeout timer-id))
  (let [timer-id (js/setTimeout f delay)]
    (reset! completions-timer timer-id)))

(defn get-root-element [id]
  (gdom/getElement (str "cand-" id)))

(defn container-at-cursor []
  (.-startContainer (.getRangeAt (js/getSelection) 0)))

(defn text-at-cursor []
  (.-textContent (container-at-cursor)))

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
  (gdom/removeChildren (get-root-element id)))

(defn use-candidate [id]
  (when-let [parent (get-root-element id)]
    (when-let [selected (find-selected-candidate parent)]
      (when-let [target (container-at-cursor)]
        (let [text (gdom/getTextContent selected)]
          (gdom/setTextContent target text)
          (.setStart (.getRangeAt (js/getSelection) 0)
                     target
                     (count text))
          (gdom/removeChildren parent))))))

(defn candidate-class [type]
  (if (= type :keyword)
    "ra-keyword"
    "ra-symbol"))

(defn make-candidate [subj]
  (crate/html [:span {:class (candidate-class (:type subj))}
               (:candidate subj)]))

(def max-completions 16)

(defn show [id candidates]
 (let [tail (if (> (count candidates) max-completions) "..." "")
       target (get-root-element id)]
   (gdom/removeChildren target)
   (when (not-empty candidates)
     (doseq [c (take max-completions candidates)]
       (.append target (make-candidate c))
       (.append target " "))
     (.append target tail)
     (select-candidate (gdom/getFirstElementChild target)))))

(defn initiate [id]
  (nrepl/send-completions (text-at-cursor)
                          #(show id %)))

(defn schedule-completions [id]
  (schedule #(initiate id) 1000))

(defn plug [input id]
  (.addEventListener input
                     "input"
                     #(schedule-completions id)))
