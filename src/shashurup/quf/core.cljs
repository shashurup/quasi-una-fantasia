(ns ^:figwheel-hooks shashurup.quf.core
  (:use-macros
   [crate.def-macros :only [defpartial]])
  (:require
   [shashurup.quf.editor :as editor]
   [shashurup.quf.assistant :as assistant]
   [shashurup.quf.nrepl :as nrepl]
   [shashurup.quf.render :refer [render]]
   [shashurup.quf.utils :as u]
   [goog.dom :as gdom]
   [goog.dom.classlist :as gcls]
   [goog.events :as gevents]
   [goog.style :as gst]
   [crate.core :as crate]
   [clojure.string :as s]
   [cljs.pprint :as pp]))

(defonce app-state (atom {:selection []}))

(defonce cell-id (atom 0))

(defn checkbox-changed [event id]
  (let [checkbox (.-target event)]
    (swap! app-state update :selection conj [id
                                             (.-value checkbox)
                                             (.-checked checkbox)])))

(defn uncheck-cell [id]
  (swap! app-state update :selection conj [id nil false]))

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
               [:span.quf-prompt (str ns "=> ")]
               [:div {:id (str "expr-" id)
                      :class "quf-input"
                      :spellcheck "false"
                      :contenteditable "true"}]
               [:div.quf-candidates {:id (str "cand-" id)}]
               [:div.quf-doc {:id (str "doc-" id)}]
               [:div {:id (str "out-" id)}]
               [:div.quf-result {:id (str "result-" id)}]]))

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
            (.focus)
            (editor/plug)
            (assistant/plug id)
            (.addEventListener "keydown" keydown))))
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

(defn delete-cell [id]
  (let [next-input (find-next-input id)
        prev-input (find-prev-input id)]
    (gdom/removeNode (get-cell-element id))
    (uncheck-cell id)
    (cond
      next-input (.focus next-input)
      prev-input (.focus prev-input)
      :else (append-cell))))

(defn delete-all []
  (doall (->> (gdom/getElementsByClass "quf-cell")
              array-seq
              (map gdom/removeNode)))
  (append-cell))

(defmulti apply-event :type)

(defmethod apply-event :default [_])

(defmethod apply-event :require [{:keys [ns]}]
  (goog/require ns))

(defn process-events [result]
  (let [processed-event-count (or (:processed-event-count @app-state) 0)
        event-queue-size (:event-queue-size result)
        apply-events (fn [response]
                       (let [events (:events (first response))]
                         (swap! app-state
                                assoc
                                :processed-event-count
                                (+ processed-event-count (count events)))
                         (doall (map apply-event events))
                         ))]
    (when (> event-queue-size processed-event-count)
      (nrepl/send-op {:op "events"
                      :from processed-event-count}
                     apply-events))))

(defn process-tab []
  (when-let [name (:name @app-state)]
    (nrepl/send-op {:op "store-config"
                    :name (str "tabs/" name)
                    :config (map gdom/getTextContent
                                 (gdom/getElementsByClass "quf-input"))}
                   identity)))

(defn out-class [line]
  (cond
    (:out line) "quf-out"
    (:err line) "quf-err"
    (:ex line)  "quf-ex"))

(defn hook-checkboxes [id]
  (doall (map (fn [el] (.addEventListener el "click" #(checkbox-changed % id)))
              (gdom/getElementsByClass "quf-check" (get-result-element id)))))

(defn apply-result [id result go-next]
  (let [valdiv (get-result-element id)
        outdiv (get-out-element id)
        success (not-any? :ex (:out result))]
    (process-events result)
    (process-tab)
    (gdom/removeChildren outdiv)
    (apply gdom/append
           outdiv
           (for [line (:out result)]
             (crate/html [:p {:class (out-class line)}
                          (s/join ", " (vals line))])))
    (gdom/removeChildren valdiv)
    (uncheck-cell id)
    (mapv #(render-result % valdiv) (:value result))
    (hook-checkboxes id)
    (.scrollIntoView (get-cell-element id))
    (when (and success go-next)
      (focus-next-cell id))))

(defn create-progress-bar [id]
  (let [handler-call (str "shashurup.quf.core.interrupt_eval(" id ")")]
    (crate/html [:div [:progress]
                 " "
                 [:input {:type "button"
                          :value "Cancel"
                          :onclick handler-call}]])))

(defn selection-updates []
  (let [[{sel :selection} _] (swap-vals! app-state update :selection (constantly []))]
    (when (not-empty sel)
      {:selection-updates sel})))

(defn eval-cell
  ([id] (eval-cell id true))
  ([id go-next]
   (doto (get-result-element id)
     (gdom/appendChild (create-progress-bar id)))
   (let [expr (-> (get-input-element id)
                  .-textContent
                  (s/replace \u00a0 " "))]
     (let [req-id (nrepl/send-eval expr
                                   #(apply-result id % go-next)
                                   (selection-updates))]
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

(def result-height-cycle {"" "quf-result-tall"
                          "quf-result-tall" "quf-result-collapsed"
                          "quf-result-collapsed" ""})

(defn cycle-result-height [id]
  (u/cycle-style (get-result-element id) result-height-cycle))

(defn hide-tabname []
  (gst/setStyle
   (gdom/getElement "quf-tabname-modal")
   "display"
   "none"))

(defn fill-tablist [resp]
  (let [datalist (gdom/getElement "quf-tablist")]
    (gdom/removeChildren datalist)
    (doall (for [name (:names (first resp))]
             (gdom/appendChild datalist
                               (crate/html [:option name]))))))

(defn show-tabname []
  (nrepl/send-op {:op "ls-config" :name "tabs"} fill-tablist)
  (gst/setStyle
   (gdom/getElement "quf-tabname-modal")
   "display"
   "block")
  (.focus (gdom/getElement "quf-tabname")))

(defn load-cells [resp]
  (when-let [cells (:config (first resp))]
    (doall (for [expr cells]
             (insert-cell nil nil expr)))))

(defn set-tabname []
  (let [name (.-value (gdom/getElement "quf-tabname"))]
    (when (not-empty name)
      (nrepl/send-op {:op "load-config" :name (str "tabs/" name)}
                     load-cells)
      (swap! app-state assoc :name name)
      (gdom/setTextContent (first (gdom/getElementsByTagName "title"))
                           (str name " - Quasi una fantasia"))))
  (hide-tabname))

(def tabname-key-map {"Enter" set-tabname
                      "Escape" hide-tabname})

(defn on-tabname-key [e]
  (when-let [f (tabname-key-map (key-event->str e))]
    (f)
    (.preventDefault e)))

(.addEventListener (gdom/getElement "quf-tabname")
                   "keydown"
                   on-tabname-key)

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
                   "C-S-l" delete-all
                   "C-j" focus-next-cell
                   "C-k" focus-prev-cell
                   "C-r" assistant/initiate-history
                   "C-h" assistant/toggle-doc
                   "C-a" move-cursor-at-start
                   "C-e" move-cursor-at-end
                   "C-s" cycle-result-height
                   "C-=" show-tabname
                   "C-t" new-tab
                   "C-m" show-checkboxes
                   "C-;" editor/sexp-mode})

(def completions-key-map {"Escape" assistant/clear-candidates
                          "C-j" assistant/use-next-candidate
                          "C-k" assistant/use-prev-candidate})

(def sexp-mode-key-map {"i" editor/insert-mode
                        "d" editor/delete
                        "c" editor/change
                        "h" editor/move-back
                        "l" editor/move-forward
                        "w" editor/next-element
                        "b" editor/prev-element
                        "v" editor/extend-selection})

(defn get-key-map [id]
  (merge cell-key-map
         (when (assistant/active id)
           completions-key-map)
         (when (editor/sexp-mode? id)
           sexp-mode-key-map)))

(gevents/listen js/window
                "load"
                (fn [_] (append-cell)))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
