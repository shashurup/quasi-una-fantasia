(ns ^:figwheel-hooks shashurup.quf.core
  (:use-macros
   [crate.def-macros :only [defpartial]])
  (:require
   [shashurup.quf.assistant :as assistant]
   [shashurup.quf.editor :as editor]
   [shashurup.quf.history :as history]
   [shashurup.quf.keymap :as keymap]
   [shashurup.quf.markup :as markup]
   [shashurup.quf.nrepl :as nrepl]
   [shashurup.quf.render :refer [eval-reply-handler render]]
   [shashurup.quf.storage :as storage]
   [shashurup.quf.utils :as u]
   [shashurup.quf.vars :as vars]
   [goog.dom :as gdom]
   [goog.dom.classlist :as gcls]
   [goog.events :as gevents]
   [goog.style :as gst]
   [crate.core :as crate]
   [clojure.string :as s]
   [cljs.pprint :as pp]))

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

(defn create-cell [id ns]
  (crate/html [:div.quf-cell {:id (str "cell-" id)}
               [:span.quf-prompt (str id "|" ns "=> ")]
               [:div {:id (str "expr-" id)
                      :class "quf-input"
                      :spellcheck "false"
                      :contenteditable "true"}]
               [:div.quf-candidates {:id (str "cand-" id)}]
               [:div.quf-doc {:id (str "doc-" id)}]
               [:div {:id (str "out-" id)}]
               [:div.quf-result {:id (str "result-" id)}]]))

(declare append-cell)

(defn insert-cell [dir el expr-text]
  (let [ns (nrepl/get-ns)]
    (if ns
      (let [id (new-cell-id)
            cell (create-cell id ns)
            keydown (keymap/keydown-handler-for id)]
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

(defn insert-cell-before
  "Insert a cell above."
  {:keymap/key :insert-cell-above}
  [before-id]
  (insert-cell :before (get-cell-element before-id) nil))

(defn insert-cell-after
  "Insert a cell below."
  {:keymap/key :insert-cell-below}
  [after-id]
  (insert-cell :after (get-cell-element after-id) nil))

(defn copy-cell-with-expr
  "Copy current cells input into a new cell below."
  {:keymap/key :copy-cell-with-expr}
  [cell-id]
  (insert-cell :after
               (get-cell-element cell-id)
               (gdom/getTextContent (get-input-element cell-id))))

(defn append-cell-with-expr
  "Copy current cells input into a new cell at the end."
  {:keymap/key :append-cell-with-expr}
  [cell-id]
  (insert-cell nil nil (.-textContent (get-input-element cell-id))))

(defn find-next-input-by-node [cell-node]
  (when-let [next (gdom/getNextElementSibling cell-node)]
    (when-let [target (gdom/getElementByClass "quf-input" next)]
      (if (gst/isElementShown target)
        target
        (find-next-input-by-node next)))))

(defn find-prev-input-by-node [cell-node]
  (when-let [prev (gdom/getPreviousElementSibling cell-node)]
    (when-let [target (gdom/getElementByClass "quf-input" prev)]
      (if (gst/isElementShown target)
        target
        (find-prev-input-by-node prev)))))

(defn find-next-input [id]
  (find-next-input-by-node (get-cell-element id)))

(defn find-prev-input [id]
  (find-prev-input-by-node (get-cell-element id)))

(defn focus-next-cell
  "Move to the cell below."
  {:keymap/key :cell-below}
  [id]
  (if-let [el (find-next-input id)]
    (.focus el)
    (append-cell)))

(defn focus-prev-cell
  "Move to the cell above."
  {:keymap/key :cell-above}
  [id]
  (if-let [el (find-prev-input id)]
    (.focus el)
    (insert-cell-before id)))

(defn store-cell-exprs []
  (let [name (:ns @nrepl/state)]
    (when (not= name "user")
      (storage/store-ns-exprs name
                              (->> (gdom/getElementsByClass "quf-input")
                                   (map #(.-textContent %))
                                   (remove empty?))))))

(defn delete-cell
  "Delete a cell."
  {:keymap/key :delete-cell}
  [id]
  (let [next-input (find-next-input id)
        prev-input (find-prev-input id)]
    (gdom/removeNode (get-cell-element id))
    (.requestIdleCallback js/window store-cell-exprs)
    (cond
      next-input (.focus next-input)
      prev-input (.focus prev-input)
      :else (append-cell))))

(defn delete-all
  "Delete all cells."
  {:keymap/key :delete-all}
  []
  (doall (->> (gdom/getElementsByClass "quf-cell")
              array-seq
              (map gdom/removeNode)))
  (.requestIdleCallback js/window store-cell-exprs)
  (append-cell))

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
    (.scrollIntoView (get-cell-element id))
    (.requestIdleCallback js/window update-title)
    (.requestIdleCallback js/window store-cell-exprs)
    (.dispatchEvent js/document (js/Event. "evalComplete"))
    (when (and go-next
               (.hasChildNodes
                (get-result-element id)))
      (focus-next-cell id))))

(defn eval-cell
  "Evaluate cell expression and display a result."
  {:keymap/key :eval-cell}
  ([id] (eval-cell id true))
  ([id go-next]
   (doto (get-result-element id)
     (gdom/removeChildren)
     (gdom/appendChild (create-progress-bar id)))
   (doto (get-out-element id)
     (gdom/removeChildren))
   (let [expr (-> (get-input-element id)
                  vars/replace-client-vars
                  (s/replace \u00a0 " "))]
     (.requestIdleCallback js/window
                           #(history/log expr))
     (let [req-id (nrepl/send-eval expr
                                   #(handle-eval-reply id % go-next)
                                   (vars/read-pending-updates!))]
       (swap! app-state assoc-in [:requests (str id)] req-id)))))

(defn eval-cell-and-stay
  "Evaluate cell expression, display a result and stay on the input field."
  {:keymap/key :eval-cell-and-stay}
  [id]
  (eval-cell id false))

(defn interrupt-eval [id]
  (nrepl/send-interrupt (get-in @app-state [:requests (str id)])))

(def result-height-cycle {"" "quf-result-tall"
                          "quf-result-tall" "quf-result-collapsed"
                          "quf-result-collapsed" ""})

(defn cycle-result-height
  "Cycles result height between full/medium/short"
  {:keymap/key :cycle-result-height}
  [id]
  (u/cycle-style (get-result-element id) result-height-cycle))

(defn populate-cells [exprs]
  (doseq [expr exprs] (insert-cell nil nil expr)))

(defn load-ns-dialog
  "Show stored namespace list dialog and populate cells."
  {:keymap/key :load-ns-dialog}
  []
  (let [dialog (crate/html [:dialog.ns-dialog
                            "Select a namespace"
                            [:select#ns-selector {:autofocus true}
                             (for [n (storage/stored-nses)]
                               [:option (str n)])]
                            [:button#ok-ns "Ok"]
                            [:button#cancel-ns "Cancel"]])]
    (.append (.-body js/document) dialog)
    (gevents/listen dialog "close" #(.remove dialog))
    (gevents/listen (gdom/getElement "ok-ns")
                    "click" #(let [choice (.-value (gdom/getElement "ns-selector"))]
                               (populate-cells (storage/load-ns-exprs choice))
                               (.close dialog)))
    (gevents/listen (gdom/getElement "cancel-ns")
                    "click" #(.close dialog))
    (.showModal dialog)))

(defmethod render :cells [subj]
  (let [exprs (if (symbol? subj)
                (storage/load-ns-exprs subj)
                (markup/top-level-forms (first subj)))]
    (populate-cells exprs)
    [:span.quf (str (count exprs) " cells loaded")]))

(defn new-tab
  "Open a new tab."
  {:keymap/key :new-tab}
  []
  (.open js/window (.-location js/window)))

(defn show-checkboxes
  "Show checkboxes on cell's result items."
  {:keymap/key :show-checkboxes}
  [id]
  (when-let [container (gdom/getElementByClass "quf-container"
                                               (get-result-element id))]
    (gcls/add container "quf-visible-checks")))

(defn hide-input
  "Hide the prompt and the input field of the cell."
  {:keymap/key :hide-input}
  [id]
  (let [input (get-input-element id)
        prompt (.-previousElementSibling input)]
    (gst/setElementShown input false)
    (gst/setElementShown prompt false)
    (focus-next-cell id)))

(defonce startup-dummy
  (do
    (swap! eval-reply-handler wrap-module-handler)
    (when (u/module? :theme)
      (u/load-module :theme))
    (keymap/register-fns! [(ns-interns 'shashurup.quf.core)
                           (ns-interns 'shashurup.quf.assistant)
                           (ns-interns 'shashurup.quf.editor)
                           (ns-interns 'shashurup.quf.vars)])
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
