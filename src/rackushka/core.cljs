(ns ^:figwheel-hooks rackushka.core
  (:use-macros
   [crate.def-macros :only [defpartial]])
  (:require
   [rackushka.editor :as editor]
   [rackushka.assistant :as assistant]
   [rackushka.highlight :as highlight]
   [rackushka.nrepl :as nrepl]
   [rackushka.render :refer [render]]
   [rackushka.utils :as u]
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [crate.core :as crate]
   [clojure.string :as s]
   [cljs.pprint :as pp]))

(defonce app-state (atom {}))

(defonce cell-id (atom 0))

(defn new-cell-id []
  (swap! cell-id inc))

(defn get-app-element []
  (gdom/getElement "app"))

(defn get-cell-element [id]
  (gdom/getElement (str "cell-" id)))

(defn get-input-element [id]
  (gdom/getElement (str "expr-" id)))

(defn get-result-element [id]
  (gdom/getElement (str "result-" id)))

(defn get-out-element [id]
  (gdom/getElement (str "out-" id)))

(defn create-cell [id ns]
  (crate/html [:div.ra-cell {:id (str "cell-" id)}
               [:span.ra-prompt (str ns "=> ")]
               [:div {:id (str "expr-" id)
                      :class "ra-input"
                      :spellcheck "false"
                      :contenteditable "true"}]
               [:div.ra-candidates {:id (str "cand-" id)}]
               [:div.ra-doc {:id (str "doc-" id)}]
               [:div {:id (str "out-" id)}]
               [:div.ra-result {:id (str "result-" id)}]]))

(defn render-result [val target]
  (let [r (render val)]
    (if (fn? r)
      (r target)
      (gdom/appendChild target (crate/html r)))))

(defn key-event->str [e]
  (str (when (.-altKey e) "A-")
       (when (.-ctrlKey e) "C-")
       (when (.-shiftKey e) "S-")
       (.-key e)))

(declare get-key-map)
(declare append-cell)

(defn insert-cell [dir el expr-text]
  (let [ns (nrepl/get-ns)]
    (if ns
      (let [id (new-cell-id)
            cell (create-cell id ns)
            keydown (fn [e]
                      (.log js/console "key " (key-event->str e))
                      (when-let [f (get (get-key-map id) (key-event->str e))]
                        (.log js/console "keymap match " (key-event->str e))
                        (f id)
                        (.preventDefault e)))]
        (condp = dir
          :before (gdom/insertSiblingBefore cell el)
          :after (gdom/insertSiblingAfter cell el)
          (gdom/appendChild (get-app-element) cell))
        (let [expr-input (get-input-element id)]
          (when (not-empty expr-text)
            (gdom/setTextContent expr-input expr-text))
          (doto expr-input
            (editor/plug)
            (highlight/plug)
            (assistant/plug id)
            (.addEventListener "keydown" keydown)
            (.focus))))
      (nrepl/send-eval "*ns*" #(append-cell)))))

(defn append-cell [] (insert-cell nil nil nil))

(defn insert-cell-before [before-id]
  (insert-cell :before (get-cell-element before-id) nil))

(defn insert-cell-after [after-id]
  (insert-cell :after (get-cell-element after-id) nil))

(defn copy-cell-with-expr [cell-id]
  (insert-cell :after
               (get-cell-element cell-id)
               (gdom/getTextContent (get-input-element cell-id))))

(defn append-cell-with-expr [cell-id]
  (insert-cell nil nil (gdom/getTextContent (get-input-element cell-id))))

(defn find-next-input [id]
  (when-let [this (get-cell-element id)]
    (when-let [next (gdom/getNextElementSibling this)]
      (gdom/getElementByClass "ra-input" next))))

(defn find-prev-input [id]
  (when-let [this (get-cell-element id)]
    (when-let [next (gdom/getPreviousElementSibling this)]
      (gdom/getElementByClass "ra-input" next))))

(defn focus-next-cell [id]
  (if-let [el (find-next-input id)]
    (.focus el)
    (append-cell)))

(defn focus-prev-cell [id]
  (if-let [el (find-prev-input id)]
    (.focus el)
    (insert-cell-before id)))

(defn delete-cell [id]
  (let [next-input (find-next-input id)
        prev-input (find-prev-input id)]
    (gdom/removeNode (get-cell-element id))
    (cond
      next-input (.focus next-input)
      prev-input (.focus prev-input)
      :else (append-cell))))

(defn delete-all []
  (doall (->> (gdom/getElementsByClass "ra-cell")
              array-seq
              (map gdom/removeNode)))
  (append-cell))

(defn out-class [line]
  (cond
    (:out line) "ra-out"
    (:err line) "ra-err"
    (:ex line)  "ra-ex"))

(defn apply-result [id result go-next]
  (let [valdiv (get-result-element id)
        outdiv (get-out-element id)
        success (not-any? :ex (:out result))]
    (gdom/removeChildren outdiv)
    (apply gdom/append
           outdiv
           (for [line (:out result)]
             (crate/html [:p {:class (out-class line)}
                          (s/join ", " (vals line))])))
    (gdom/removeChildren valdiv)
    (mapv #(render-result % valdiv) (:value result))
    (.scrollIntoView (get-cell-element id))
    (when (and success go-next)
      (focus-next-cell id))))

(defn create-progress-bar [id]
  (let [handler-call (str "rackushka.core.interrupt_eval(" id ")")]
    (crate/html [:div [:progress]
                 " "
                 [:input {:type "button"
                          :value "Cancel"
                          :onclick handler-call}]])))

(defn eval-cell
  ([id] (eval-cell id true))
  ([id go-next]
   (doto (get-result-element id)
     (gdom/appendChild (create-progress-bar id)))
   (let [expr (-> (get-input-element id)
                  .-textContent
                  (s/replace \u00a0 " "))]
     (let [req-id (nrepl/send-eval expr #(apply-result id % go-next))]
       (swap! app-state assoc-in [:requests (str id)] req-id)))))

(defn eval-cell-and-stay [id] (eval-cell id false))

(defn get-first-text-node [node]
  (when node
    (if (= (.-nodeType node) 1)
      (get-first-text-node (.-firstChild node))
      node)))

(defn move-cursor-at-start [id]
  (let [input (get-input-element id)
        cursor-el (or (get-first-text-node (.-firstChild input)) input)]
    (doto (js/getSelection)
      (.removeAllRanges)
      (.addRange (doto (.createRange js/document)
                   (.setStart cursor-el 0)
                   (.collapse true))))))

(defn move-cursor-at-end [id]
  (let [input (get-input-element id)
        cursor-el (or (get-first-text-node (.-lastChild input)) input)]
    (doto (js/getSelection)
      (.removeAllRanges)
      (.addRange (doto (.createRange js/document)
                   (.setStart cursor-el (count (.-textContent cursor-el)))
                   (.collapse true))))))

(defn interrupt-eval [id]
  (nrepl/send-interrupt (get-in @app-state [:requests (str id)])))

(def result-height-cycle {"" "ra-tall"
                          "ra-tall" "ra-height-collapsed"
                          "ra-height-collapsed" ""})

(defn cycle-result-height [id]
  (u/cycle-style (get-result-element id) result-height-cycle))

(def cell-key-map {"Enter" eval-cell
                   "C-Enter" eval-cell-and-stay
                   "Tab" assistant/attempt-complete
                   "C-Delete" delete-cell
                   "C-u" delete-cell
                   "C-i" insert-cell-before
                   "C-o" insert-cell-after
                   "C-y" copy-cell-with-expr
                   "C-S-Y" append-cell-with-expr
                   "C-l" delete-all
                   "C-j" focus-next-cell
                   "C-k" focus-prev-cell
                   "C-r" assistant/initiate-history
                   "C-h" assistant/toggle-doc
                   "C-a" move-cursor-at-start
                   "C-e" move-cursor-at-end
                   "C-s" cycle-result-height})

(def completions-key-map {"Enter" assistant/use-candidate
                          "Escape" assistant/clear-candidates
                          "Tab" assistant/select-next-candidate
                          "C-j" assistant/select-next-candidate
                          "C-k" assistant/select-prev-candidate})

(defn get-key-map [id]
  (merge cell-key-map
         (when (assistant/active id)
           completions-key-map)))

(gevents/listen js/window
                "load"
                (fn [_] (append-cell)))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
