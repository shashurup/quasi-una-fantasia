(ns ^:figwheel-hooks rackushka.core
  (:use-macros
   [crate.def-macros :only [defpartial]])
  (:require
   [rackushka.editor :as editor]
   [rackushka.completions :as completions]
   [rackushka.desc :as desc]
   [rackushka.highlight :as highlight]
   [rackushka.nrepl :as nrepl]
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [crate.core :as crate]
   [clojure.string :as s]
   [cljs.pprint :as pp]))

(defonce app-state (atom {}))

(defonce check-id (atom 0))

(defn new-check-id []
  (swap! check-id inc))

(defonce cell-id (atom 0))

(defn new-cell-id []
  (swap! cell-id inc))

(defn get-app-element []
  (gdom/getElement "app"))

(defn get-cell-element [id]
  (gdom/getElement (str "cell-" id)))

(defn get-input-element [id]
  (gdom/getElement (str "expr-" id)))


;; Tree

(defn create-cell [id ns]
  (crate/html [:div.ra-cell {:id (str "cell-" id)}
               [:span.ra-prompt (str ns "=> ")]
               [:div {:id (str "expr-" id)
                      :class "ra-input"
                      :spellcheck "false"
                      :contenteditable "true"}]
               [:div.ra-candidates {:id (str "cand-" id)}]
               [:div {:id (str "out-" id)}]
               [:div.ra-result {:id (str "result-" id)}]]))

(defn make-scalar [class val]
  [:span {:class class} (pr-str val)])

(defmulti render (fn [subj]
                   (if-let [hint (:rackushka/hint (meta subj))]
                     (if (keyword? hint)
                       hint
                       (first hint))
                     (type subj))))

(defmethod render :default [subj]
  [:span.ra (pr-str subj)])

(defmethod render nil [subj]
  (make-scalar "ra-nil" subj))

(defmethod render js/Number [subj]
  (make-scalar "ra-number" subj))

(defmethod render js/String [subj]
  (make-scalar "ra-string" subj))

(defmethod render js/Boolean [subj]
  (make-scalar "ra-bool" subj))

(defmethod render Keyword [subj]
  (make-scalar "ra-keyword" subj))

(defmethod render Symbol [subj]
  (make-scalar "ra-symbol" subj))

(def paren-map {:map    ["{" "}"]
                :vector ["[" "]"]
                :list   ["(" ")"]
                :set    ["#{" "}"]})

(defn make-composite [subj cont-type render-fn]
  (let [check-id (new-check-id)
        [prefix suffix] (get paren-map cont-type)]
    [:div.ra-composite-wrapper
     [:label {:for check-id} prefix]
     [:input {:id check-id
              :type "checkbox"
              :style "display: none"}]
     [:div {:class (str "ra-composite-body-"
                        (subs (str cont-type) 1))}
      (for [node subj] (render-fn node))]
     [:label.ra-ellipsis {:for check-id} "..."]
     [:span.ra-closing-paren suffix]]))

(defmethod render PersistentVector [subj]
  (make-composite subj :vector render))

(defmethod render List [subj]
  (make-composite subj :list render))

(defmethod render PersistentHashSet [subj]
  (make-composite subj :set render))

(defn render-map-entry [[k v]]
  [:div.ra-map-entry (render k) (render v)])

(defn make-map [subj]
  (make-composite subj :map render-map-entry))

(defmethod render PersistentArrayMap [subj]
  (make-map subj))

(defmethod render PersistentHashMap [subj]
  (make-map subj))

(defmethod render :text [subj]
  [:pre (s/join "\n" subj)])

(defmethod render :html [subj] subj)

(defmethod render :tag [[tag arg]]
  [:span {:class "ra-tag"} (str "#" tag " " arg)])

;; Table

(defmulti render-cell type)

(defmethod render-cell :default [value]
  [:td.ra (if (coll? value)
            (if (and (= :tag (:rackushka/hint (meta value)))
                     (= 'object (first value)) )
              (last (second value))
              (render value))
            (pr-str value))])

(defmethod render-cell nil [_] [:td.ra])

(defmethod render-cell js/String [value]
  [:td {:class (str "ra " "ra-string-cell")} value])

(defmethod render-cell js/Number [value]
  [:td {:class (str "ra " "ra-number-cell")}
   (.format (js/Intl.NumberFormat.) value)])

(defmethod render-cell js/Date [value]
  [:td {:class (str "ra " "ra-date-cell")}
   (.toISOString value)])

(defn guess-columns [data]
  (let [row (first data)]
    (if (map? row)
      (keys row)
      (range (count row)))))

(defmethod render :table [data]
  (let [hint (:rackushka/hint (meta data))
        [names rndrs] (if (keyword? hint)
                        (desc/table-desc (guess-columns data))
                        (when (coll? hint)
                          (let [sec (second hint)]
                            (if (keyword? sec)
                              (desc/table-desc sec (nth hint 2))
                              (desc/table-desc sec)))))]
    [:table.ra [:thead [:tr (for [name names]
                              [:th.ra name])]]
               [:tbody (for [row data]
                         [:tr (for [rndr rndrs]
                                (render-cell (rndr row)))])]]))

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
            (completions/plug id)
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
  (let [valdiv (gdom/getElement (str "result-" id))
        outdiv (gdom/getElement (str "out-" id))
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
   (doto (gdom/getElement (str "result-" id))
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

(def cell-key-map {"Enter" eval-cell
                   "C-Enter" eval-cell-and-stay
                   "Tab" completions/initiate-at-point
                   "C-Delete" delete-cell
                   "C-u" delete-cell
                   "C-i" insert-cell-before
                   "C-o" insert-cell-after
                   "C-y" copy-cell-with-expr
                   "C-S-Y" append-cell-with-expr
                   "C-l" delete-all
                   "C-j" focus-next-cell
                   "C-k" focus-prev-cell
                   "C-r" completions/initiate-history
                   "C-a" move-cursor-at-start
                   "C-e" move-cursor-at-end})

(def completions-key-map {"Enter" completions/use-candidate
                          "Escape" completions/clear-candidates
                          "Tab" completions/select-next-candidate
                          "C-j" completions/select-next-candidate
                          "C-k" completions/select-prev-candidate})

(defn get-key-map [id]
  (merge cell-key-map
         (when (completions/active id)
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
