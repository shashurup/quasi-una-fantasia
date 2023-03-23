(ns ^:figwheel-hooks rackushka.core
  (:use-macros
   [crate.def-macros :only [defpartial]])
  (:require
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

(defn out-class [line]
  (cond
    (:out line) "ra-out"
    (:err line) "ra-err"
    (:ex line)  "ra-ex"))

(declare add-new-cell)

(defn find-next-input [id]
  (->> (gdom/getElementsByClass "ra-input")
       array-seq
       (drop-while #(not= (.-id %)
                          (str "expr-" id)))
       second))

(defn last-input []
  (last (array-seq (gdom/getElementsByClass "ra-input"))))

(defn focus-next-cell [id]
  (if-let [el (find-next-input id)]
    (.focus el)
    (add-new-cell)))

(defn render-result [val target]
  (let [r (render val)]
    (if (fn? r)
      (r target)
      (gdom/appendChild target (crate/html r)))))

(defn apply-result [id result go-next]
  (let [valdiv (gdom/getElement (str "result-" id))
        outdiv (gdom/getElement (str "out-" id))]
    (gdom/removeChildren outdiv)
    (apply gdom/append
           outdiv
           (for [line (:out result)]
             (crate/html [:p {:class (out-class line)}
                          (s/join ", " (vals line))])))
    (.scrollIntoView outdiv)
    (gdom/removeChildren valdiv)
    (mapv #(render-result % valdiv) (:value result))
    (.scrollIntoView valdiv)
    (when go-next
      (focus-next-cell id))))

(defn eval-cell
  ([id] (eval-cell id true))
  ([id go-next]
   (doto (gdom/getElement (str "result-" id))
     (gdom/appendChild (crate/html [:progress])))
   (let [expr (-> (gdom/getElement (str "expr-" id))
                  .-textContent
                  (s/replace \u00a0 " "))]
     (nrepl/send-eval expr #(apply-result id % go-next)))))

(defn eval-cell-and-stay [id] (eval-cell id false))

(defn delete-cell [id]
  (let [next-input (find-next-input id)]
    (gdom/removeNode (gdom/getElement (str "cell-" id)))
    (.focus (or next-input (last-input)))))

(defn delete-all []
  (doall (->> (gdom/getElementsByClass "ra-cell")
              array-seq
              (map gdom/removeNode)))
  (add-new-cell))

(def cell-key-map {"Enter" eval-cell
                   "C-Enter" eval-cell-and-stay
                   "Tab" completions/initiate ;; doesn't work
                   "C-Delete" delete-cell
                   "C-l" delete-all})

(def completions-key-map {"Enter" completions/use-candidate
                          "Escape" completions/clear-candidates ;; doesn't work
                          "C-j" completions/select-next-candidate
                          "C-k" completions/select-prev-candidate})

(defn get-key-map [id]
  (merge cell-key-map
         (when (completions/active id)
           completions-key-map)))

(defn key-event->str [e]
  (str (when (.-altKey e) "A-")
       (when (.-ctrlKey e) "C-")
       (when (.-shiftKey e) "S-")
       (.-key e)))

(defn add-new-cell []
  (let [ns (nrepl/get-ns)]
    (if ns
      (let [id (new-cell-id)
            cell (create-cell id ns)
            keydown (fn [e]
                      (when-let [f (get (get-key-map id) (key-event->str e))]
                        (f id)
                        (.preventDefault e)))]
        (gdom/appendChild (get-app-element) cell)
        (let [expr-input (gdom/getElement (str "expr-" id))]
          (doto expr-input
            (highlight/plug)
            (completions/plug id)
            (.addEventListener "keydown" keydown)
            (.focus))))
      (nrepl/send-eval "*ns*" #(add-new-cell)))))

(gevents/listen js/window
                "load"
                (fn [_] (add-new-cell)))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
