(ns ^:figwheel-hooks shashurup.quf.core
  (:use-macros
   [crate.def-macros :only [defpartial]])
  (:require
   [shashurup.quf.assistant :as assistant]
   [shashurup.quf.editor :as editor]
   [shashurup.quf.history :as history]
   [shashurup.quf.nrepl :as nrepl]
   [shashurup.quf.render :refer [eval-reply-handler]]
   [shashurup.quf.utils :as u]
   [goog.dom :as gdom]
   [goog.dom.classlist :as gcls]
   [goog.events :as gevents]
   [goog.style :as gst]
   [crate.core :as crate]
   [clojure.string :as s]
   [cljs.pprint :as pp]
   [cljs.tools.reader :refer [read-string]]))

(u/begin-module-load! :core)

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

(defn checkbox-changed [event id]
  (let [checkbox (.-target event)
        upd [id (.-value checkbox) (.-checked checkbox)]]
    (nrepl/send-update-vars [['*selection* 'update-selection upd]
                             ['*s 'set-current-selection]])))

(defn something-checked? [id]
  (when-let [el (get-result-element id)]
    (some (fn [ch] (.-checked ch))
          (gdom/getElementsByClass "quf-check" el))))

(defn uncheck-cell [id]
  (when (something-checked? id)
    (nrepl/send-update-vars [['*selection*
                              'update-selection
                              [id nil false]]])))

(defn nearest-cell-selection
  "Checked items from the nearest upper cell with checkboxes"
  [input-node]
  (->> (.-parentElement input-node)
       (iterate #(.-previousElementSibling %))
       (take-while identity)
       rest
       (filter #(gdom/getElementByClass "quf-check" %))
       first
       (gdom/getElementsByClass "quf-check")
       (filter #(.-checked %))
       (map #(.-value %))
       vec))

(defn all-cells-selection
  "All checked items"
  [_]
  (->> (gdom/getElementsByClass "quf-check")
       (filter #(.-checked %))
       (map #(.-value %))
       vec))

(defn all-cell-exprs
  "Vector of all cell expressions"
  [_]
  (->> (gdom/getElementsByClass "quf-input")
       (map #(.-textContent %))
       vec))

(def client-vars {"s" nearest-cell-selection
                  "s-all" all-cells-selection
                  "cells" all-cell-exprs})

(defn replace-var [text node]
  (let [var (s/trim text)]
    (if-let [f (and (s/starts-with? var "$$")
                    (get client-vars (subs var 2)))]
      (s/replace-first text var (f node))
      text)))

(defn replace-client-vars [node]
  (->> node
       editor/text-node-seq
       (map #(.-textContent %))
       (map #(replace-var % node))
       (apply str)))

(defn expand-client-vars [id]
  (let [node (get-input-element id)]
    (doseq [text-node (editor/text-node-seq node)]
      (.replaceWith text-node
                    (replace-var (.-textContent text-node)
                                 node)))
    (editor/restructure node)))

(defn create-cell [id ns]
  (crate/html [:div.quf-cell {:id (str "cell-" id)}
               [:span.quf-prompt (str ns "=> ")]
               [:div {:id (str "expr-" id)
                      :class "quf-input"
                      :spellcheck "false"
                      :contenteditable "true"}]
               [:div.quf-candidates {:id (str "cand-" id)}]
               [:div.quf-doc {:id (str "doc-" id)}]
               [:div {:id (str "out-" id)}]
               [:div.quf-result {:id (str "result-" id)}]]))

(defn key-event->str [e]
  (str (when (.-altKey e) "A-")
       (when (.-ctrlKey e) "C-")
       (when (.-shiftKey e) "S-")
       (.-key e)))

(declare find-key-binding)
(declare append-cell)

(defn insert-cell [dir el expr-text]
  (let [ns (nrepl/get-ns)]
    (if ns
      (let [id (new-cell-id)
            cell (create-cell id ns)
            keydown (fn [e]
                      (when-let [f (find-key-binding id (key-event->str e))]
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
            (.focus)
            (editor/plug)
            (assistant/plug id)
            (.addEventListener "keydown" keydown))))
      (nrepl/send-clone (fn [_]
                          (nrepl/send-eval "*ns*"
                                           #(when (:value %)
                                              (append-cell))))))))

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
  (insert-cell nil nil (.-textContent (get-input-element cell-id))))

(defn find-next-input [id]
  (when-let [this (get-cell-element id)]
    (when-let [next (gdom/getNextElementSibling this)]
      (gdom/getElementByClass "quf-input" next))))

(defn find-prev-input [id]
  (when-let [this (get-cell-element id)]
    (when-let [next (gdom/getPreviousElementSibling this)]
      (gdom/getElementByClass "quf-input" next))))

(defn focus-next-cell [id]
  (if-let [el (find-next-input id)]
    (.focus el)
    (append-cell)))

(defn focus-prev-cell [id]
  (if-let [el (find-prev-input id)]
    (.focus el)
    (insert-cell-before id)))

(defn store-cell-exprs []
  (let [name (:ns @nrepl/state)]
    (when (not= name "user")
      (.setItem (.-localStorage js/window)
                (str "ns." name)
                (pr-str (->> (gdom/getElementsByClass "quf-input")
                             (map #(.-textContent %))
                             (remove empty?)))))))

(defn delete-cell [id]
  (let [next-input (find-next-input id)
        prev-input (find-prev-input id)]
    (uncheck-cell id)
    (gdom/removeNode (get-cell-element id))
    (.requestIdleCallback js/window store-cell-exprs)
    (cond
      next-input (.focus next-input)
      prev-input (.focus prev-input)
      :else (append-cell))))

(defn delete-all []
  (doall (->> (gdom/getElementsByClass "quf-cell")
              array-seq
              (map gdom/removeNode)))
  (.requestIdleCallback js/window store-cell-exprs)
  (append-cell))

(defn hook-checkboxes [id]
  (doall (map (fn [el] (.addEventListener el "click" #(checkbox-changed % id)))
              (gdom/getElementsByClass "quf-check" (get-result-element id)))))

(defn create-progress-bar [id]
  (let [handler-call (str "shashurup.quf.core.interrupt_eval(" id ")")]
    (crate/html [:div {:id (str "progress-" id)}
                 [:progress] " "
                 [:input {:type "button"
                          :value "Cancel"
                          :onclick handler-call}]])))

(defn get-progress-element [id]
  (gdom/getElement (str "progress-" id)))

(defn remove-progress-bar [id]
  (gdom/removeNode (get-progress-element id)))

(defn wrap-module-handler [handler]
  (fn [id {:keys [out] :as reply}]
    (let [data (nrepl/try-read-value-with-meta out)]
      (if (= (:shashurup.quf/hint (meta data)) :module)
        (doseq [ns data] (u/load-module ns))
        (handler id reply)))))

(defonce title (.-textContent (first (gdom/getElementsByTagName "title"))))

(defn update-title []
  (when-let [title-el (first (gdom/getElementsByTagName "title"))]
    (gdom/setTextContent title-el (str (:ns @nrepl/state) " - " title))))

(defn handle-eval-reply [id
                         {:keys [x-data status] :as reply}
                         go-next]
  (let [handler @eval-reply-handler]
    (handler id reply))
  (when (nrepl/terminated? status)
    (remove-progress-bar id)
    (hook-checkboxes id)
    (.scrollIntoView (get-cell-element id))
    (.requestIdleCallback js/window update-title)
    (.requestIdleCallback js/window store-cell-exprs)
    (.dispatchEvent js/document (js/Event. "evalComplete"))
    (when (and go-next
               (.hasChildNodes
                (get-result-element id)))
      (focus-next-cell id))))

(defn eval-cell
  ([id] (eval-cell id true))
  ([id go-next]
   (uncheck-cell id)
   (doto (get-result-element id)
     (gdom/removeChildren)
     (gdom/appendChild (create-progress-bar id)))
   (doto (get-out-element id)
     (gdom/removeChildren))
   (let [expr (-> (get-input-element id)
                  replace-client-vars
                  (s/replace \u00a0 " "))]
     (.requestIdleCallback js/window
                           #(history/log expr))
     (let [req-id (nrepl/send-eval expr
                                   #(handle-eval-reply id % go-next))]
       (swap! app-state assoc-in [:requests (str id)] req-id)))))

(defn eval-cell-and-stay [id] (eval-cell id false))

(defn interrupt-eval [id]
  (nrepl/send-interrupt (get-in @app-state [:requests (str id)])))

(def result-height-cycle {"" "quf-result-tall"
                          "quf-result-tall" "quf-result-collapsed"
                          "quf-result-collapsed" ""})

(defn cycle-result-height [id]
  (u/cycle-style (get-result-element id) result-height-cycle))

(defn stored-nses []
  (->> (range)
       (map #(.key (.-localStorage js/window) %))
       (take-while identity)
       (filter #(s/starts-with? % "ns."))
       (map #(subs % 3))))

(defn load-ns-exprs [name]
  (-> (.-localStorage js/window)
      (.getItem (str "ns." name))
      read-string
      (or [])))

(defn populate-cells [exprs]
  (doseq [expr exprs] (insert-cell nil nil expr)))

(defn load-ns-dialog []
  (let [dialog (crate/html [:dialog.ns-dialog
                            "Select a namespace"
                            [:select#ns-selector {:autofocus true}
                             (for [n (stored-nses)]
                               [:option n])]
                            [:button#ok-ns "Ok"]
                            [:button#cancel-ns "Cancel"]])]
    (.append (.-body js/document) dialog)
    (gevents/listen dialog "close" #(.remove dialog))
    (gevents/listen (gdom/getElement "ok-ns")
                    "click" #(let [choice (.-value (gdom/getElement "ns-selector"))]
                               (populate-cells (load-ns-exprs choice))
                               (.close dialog)))
    (gevents/listen (gdom/getElement "cancel-ns")
                    "click" #(.close dialog))
    (.showModal dialog)))

(defn new-tab []
  (.open js/window (.-location js/window)))

(defn show-checkboxes [id]
  (when-let [container (gdom/getElementByClass "quf-container"
                                               (get-result-element id))]
    (gcls/add container "quf-visible-checks")))

(def cell-key-map {"Enter" eval-cell
                   "C-Enter" eval-cell-and-stay
                   "Tab" assistant/attempt-complete
                   "C-Delete" delete-cell
                   "C-u" delete-cell
                   "C-i" insert-cell-before
                   "C-o" insert-cell-after
                   "C-y" copy-cell-with-expr
                   "C-S-Y" append-cell-with-expr
                   "C-S-L" delete-all
                   "C-j" focus-next-cell
                   "C-k" focus-prev-cell
                   "C-r" assistant/initiate-history
                   "C-h" assistant/toggle-doc
                   "C-s" cycle-result-height
                   "C-=" load-ns-dialog
                   "C-t" new-tab
                   "C-m" show-checkboxes
                   "C-e" expand-client-vars
                   "C-;" editor/sexp-mode})

(def completions-key-map {"Escape" assistant/clear-candidates
                          "C-j" assistant/use-next-candidate
                          "C-k" assistant/use-prev-candidate})

(def sexp-mode-key-map {"i" editor/insert-mode
                        "d" editor/delete
                        "c" editor/change
                        "h" editor/move-back
                        "l" editor/move-forward
                        "j" editor/move-down
                        "k" editor/move-up
                        "w" editor/next-element-begin
                        "e" editor/next-element-end
                        "b" editor/prev-element
                        "v" editor/extend-selection
                        "S-(" editor/wrap-with-a-paren
                        "[" editor/wrap-with-a-bracket
                        "S-{" editor/wrap-with-a-brace
                        "S-\"" editor/wrap-with-quotes
                        "u" editor/unwrap
                        "f" editor/forward-slurp
                        "S-F" editor/forward-barf
                        "a" editor/backward-slurp
                        "S-A" editor/backward-barf
                        "S-^" editor/move-start
                        "S-$" editor/move-end
                        "0" editor/move-start
                        ;; "u" #(.execCommand js/document "undo")
                        ;; "y" #(.execCommand js/document "copy")
                        ;; "p" #(.execCommand js/document "paste")
                        })

(defn find-key-binding [id key]
  (or (when (editor/sexp-mode? id)
        (sexp-mode-key-map key))
      (when (assistant/active id)
        (completions-key-map key))
      (cell-key-map key)
      (when (editor/sexp-mode? id)
        identity)))

(defonce startup-dummy
  (do
    (swap! eval-reply-handler wrap-module-handler)
    (when (u/module? :theme)
      (u/load-module :theme))
    (gevents/listen js/window
                    "load"
                    (fn [_] (append-cell)))))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(u/set-module-loaded!)
