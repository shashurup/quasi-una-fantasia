(ns shashurup.quf.render
  (:require [clojure.string :as s]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [shashurup.quf.desc :as desc]
            [shashurup.quf.nrepl :as nrepl]
            [shashurup.quf.utils :as u]))

(defn value-type [subj]
  (if-let [hint (:shashurup.quf/hint (meta subj))]
    (if (keyword? hint)
      hint
      (first hint))
    (type subj)))

(defmulti render value-type)

(defn get-result-element [id]
  (gdom/getElement (str "result-" id)))

(defn get-out-element [id]
  (gdom/getElement (str "out-" id)))

;; Cell output handling

(def out-class-map {:out "quf-out"
                    :err "quf-err"
                    :ex "quf-ex"})

(def out-keys #{:out :err :ex})

(defn make-out-line [reply]
  (crate/html [:p {:class (some out-class-map (keys reply))}
               (some reply out-keys)]))

(def ^:dynamic defer (fn [_]))

(defn render-result [expr val target]
  (let [deferred (atom [])]
    (binding [defer #(swap! deferred conj %)]
      (let [val (if (coll? val)
                  (vary-meta val assoc ::expr expr)
                  val)
            result (render val)]
        (gdom/appendChild target (if (gdom/isElement result)
                                   result
                                   (crate/html result)))
        (doseq [f @deferred] (f))))))

(defonce cell-handlers (atom {}))
(defonce output-handlers (atom {}))

(defn render-reply [id expr {:keys [out err ex value status] :as reply}]
  (let [cell-handler (get @cell-handlers id)]
    (cond
      (contains? reply :value) (render-result expr value (get-result-element id))
      cell-handler (cell-handler id reply)
      (some reply out-keys) (let [data (nrepl/try-read-value-with-meta out)]
                              (if-let [hint (:shashurup.quf/hint (meta data))]
                                (if-let [out-handler (get @output-handlers hint)]
                                  (out-handler id data)
                                  (u/not-found-hook))
                                (gdom/append (get-out-element id)
                                             (make-out-line reply)))))))

;; Extra nrepl messages

(defn update-progress [id [message value max]]
  (let [progress-el (gdom/getElement (str "progress-" id))]
    (when value
      (when-let [el (first (gdom/getElementsByTagName "progress" progress-el))]
        (set! (.-value el) value)
        (when max
          (set! (.-max el) max))))
    (when message
      (if-let [el (first (gdom/getElementsByTagName "p" progress-el))]
        (.replaceChildren el message)
        (.insertBefore progress-el
                       (crate/html [:p message])
                       (.-firstChild progress-el))))))

;; Tree

(defonce check-id (atom 0))

(defn new-check-id []
  (swap! check-id inc))

(defn make-scalar [class val]
  [:span {:class class} (pr-str val)])

(defmethod render :default [subj]
  (u/not-found-hook)
  [:span.quf (pr-str subj)])

(defmethod render nil [subj]
  (make-scalar "quf-nil" subj))

(defmethod render js/Number [subj]
  (make-scalar "quf-number" subj))

(defmethod render js/String [subj]
  (make-scalar "quf-string" subj))

(defmethod render js/Boolean [subj]
  (make-scalar "quf-bool" subj))

(defmethod render Keyword [subj]
  (make-scalar "quf-keyword" subj))

(defmethod render Symbol [subj]
  (make-scalar "quf-symbol" subj))

(def paren-map {:map    ["{" "}"]
                :vector ["[" "]"]
                :list   ["(" ")"]
                :set    ["#{" "}"]})

(defn add-context [subj [path expr]]
  (if (coll? subj)
    (vary-meta subj merge {::path (or path [])
                           ::expr expr})
    subj))

(defn push-context [subj [path expr] el]
  (add-context subj [(conj (or path []) el) expr]))

(defn get-context [subj]
  (let [m (meta subj)]
    [(or (::path m) []) (::expr m)]))

(defn load-more-ellipsis [f]
  [:span {:style "cursor: pointer"
          :onclick (u/gen-js-call f)}
   "\u2026"])

(declare retrieve-composite-fragment)

(defn insert-composite-fragment [target ctx from resp]
  (when (contains? resp :value)
    (let [{value :value} resp
          fragment (map-indexed #(render (push-context %2 ctx (+ %1 from)))
                                value)
          parent (.-parentElement target)]
      (.remove target)
      (doseq [el fragment] (.append parent (crate/html el)))
      (when (:shashurup.quf.pruner/range (meta value))
        (.append parent (crate/html (load-more-ellipsis #(retrieve-composite-fragment % ctx))))))))

(defn retrieve-composite-fragment [e [path expr]]
  (let [target (.-target  e)
        from (dec (.-childElementCount (.-parentElement target)))
        to (+ from u/pruner-quota)]
    (nrepl/send-eval expr
                     #(insert-composite-fragment target [path expr] from %)
                     {:shashurup.quf.pruner/path path
                      :shashurup.quf.pruner/range {:from from
                                                   :to to}})))

(defn make-composite [subj cont-type render-fn]
  (let [ctx (get-context subj)
        check-id (new-check-id)
        [prefix suffix] (get paren-map cont-type)]
    [:div.quf-composite-wrapper.quf-container
     [:label {:for check-id} prefix]
     [:input {:id check-id
              :type "checkbox"
              :style "display: none"}]
     [:div {:class (str "quf-composite-body-"
                        (subs (str cont-type) 1))}
      (map-indexed #(render-fn (push-context %2 ctx %1)) subj)
      (when (:shashurup.quf.pruner/range (meta subj))
        (load-more-ellipsis #(retrieve-composite-fragment % ctx)))]
     [:label.quf-ellipsis {:for check-id} "\u2026"] ;; ellipsis
     [:span.quf-closing-paren suffix]]))

(defmethod render PersistentVector [subj]
  (make-composite subj :vector render))

(defmethod render List [subj]
  (make-composite subj :list render))

(defmethod render EmptyList [subj]
  (make-composite subj :list render))

(defmethod render PersistentHashSet [subj]
  (make-composite subj :set render))

(defn render-map-entry [entry]
  (let [[path expr] (get-context entry)
        [k v] entry]
      [:div.quf-map-entry
       (render k)
       (render (push-context v [(pop path) expr] k))]))

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
  [:div.quf-tag (str "#" tag) (render arg)])


;; Table

(defmulti render-cell type)

(defmethod render-cell :default [value]
  [:td.quf (if (coll? value)
             (if (and (= :tag (:shashurup.quf/hint (meta value)))
                      (= 'object (first value)) )
               (last (second value))
               (render value))
             (pr-str value))])

(defmethod render-cell nil [_] [:td.quf])

(defmethod render-cell js/String [value]
  [:td {:class (str "quf " "quf-string-cell")} value])

(defmethod render-cell js/Number [value]
  [:td {:class (str "quf " "quf-number-cell")}
   (.format (js/Intl.NumberFormat.) value)])

(defmethod render-cell js/Date [value]
  [:td {:class (str "quf " "quf-date-cell")}
   (.toISOString value)])

(defn guess-columns [data]
  (let [row (first data)]
    (if (map? row)
      (keys row)
      (range (count row)))))

(def col-width-cycle {"" "quf-wide"
                      "quf-wide" "quf-width-collapsed"
                      "quf-width-collapsed" ""})

(defn cycle-col-width [table col-idx]
  (let [tbody (first (gdom/getElementsByTagName "tbody" table))
        rows (gdom/getElementsByTagName "tr" tbody)]
    (doall (for [row rows]
             (u/cycle-style (nth (seq (gdom/getElementsByTagName "td" row)) col-idx)
                            col-width-cycle)))))

(defn header-click [e]
  (let [header (.-target e)
        table (u/find-parent-tag header "TABLE")
        col-idx (->> (gdom/getElementsByTagName "th" (.-parentElement header))
                     (map-indexed #(vector %1 (= %2 header)))
                     (filter second)
                     ffirst)]
    (cycle-col-width table col-idx)))

(defn render-header [name]
  (let [header (crate/html [:th.quf name])]
    (.addEventListener header "click" header-click)
    header))

(defn render-checkbox [val]
  [:input.quf-check {:type "checkbox"
                     :value val}])

(defn parse-hint [subj]
  (when (coll? subj)
    (let [sec (second subj)]
      (if (keyword? sec)
        (desc/table-desc sec (nth subj 2))
        (desc/table-desc sec)))))

(defn insert-table-fragment [target ctx resp]
  (when (contains? resp :value)
    (let [{value :value} resp
          parent (.-parentElement target)
          table (crate/html (render (add-context value ctx)))
          tbody (first (.getElementsByTagName table "tbody"))]
      (.remove target)
      (doseq [el (vec (.-children tbody))]
        (.append parent el)))))

(defn retrieve-table-fragment [e [path expr]]
  (.log js/console "retrieve" e)
  (let [target (.-parentElement (.-parentElement (.-target  e)))
        from (dec (.-childElementCount (.-parentElement target)))
        to (+ from u/pruner-quota)]
    (nrepl/send-eval expr
                     #(insert-table-fragment target [path expr] %)
                     {:shashurup.quf.pruner/path path
                      :shashurup.quf.pruner/range {:from from
                                                   :to to}})))

(defmethod render :table [data]
  (let [hint (:shashurup.quf/hint (meta data))
        ctx (get-context data)
        [names rndrs get-key] (if (keyword? hint)
                               (desc/table-desc (guess-columns data))
                               (parse-hint hint))]
    [:table.quf.quf-container
     [:thead [:tr
              (when get-key [:th.quf-check-cell])
              (for [name names]
                (render-header name))]]
     [:tbody (for [row data]
               [:tr
                (when get-key [:td.quf-check-cell (render-checkbox (get-key row))])
                (for [rndr rndrs]
                  (render-cell (rndr row)))])
      (when (:shashurup.quf.pruner/range (meta data))
        [:tr [:td (load-more-ellipsis #(retrieve-table-fragment % ctx))]])]]))

(defn render-obj [obj names rndrs show-attr-names get-key]
  [:div.quf-object 
   (for [[name rndr] (zipmap names rndrs)]
     (let [val (rndr obj)
           val-el (if (coll? val)
                    (render val)
                    [:span (str val)])]
       (if show-attr-names
         [:div.quf-map-entry [:span.quf-keyword name] "=" val-el]
         [:div.quf-object-attr val-el])))
   (when get-key (render-checkbox (get-key obj)))])

(defn insert-list-fragment [target ctx resp]
  (when (contains? resp :value)
    (let [{value :value} resp
          parent (.-parentElement target)
          lst (crate/html (render (add-context value ctx)))]
      (.remove target)
      (doseq [el (vec (.-children lst))]
        (.append parent el)))))

(defn retrieve-list-fragment [e [path expr]]
  (let [target (.-target  e)
        from (dec (.-childElementCount (.-parentElement target)))
        to (+ from u/pruner-quota)]
    (nrepl/send-eval expr
                     #(insert-list-fragment target [path expr] %)
                     {:shashurup.quf.pruner/path path
                      :shashurup.quf.pruner/range {:from from
                                                   :to to}})))

(defn render-list [data show-attr-names]
  (let [ctx (get-context data)
        hint (:shashurup.quf/hint (meta data))
        [names rndrs get-key] (parse-hint hint)]
    [:div.quf-composite-body-list.quf-container
     (for [obj data]
       (render-obj obj names rndrs show-attr-names get-key))
     (when (:shashurup.quf.pruner/range (meta data))
       (load-more-ellipsis #(retrieve-list-fragment % ctx)))]))

(defmethod render :list [data]
  (render-list data false))

(defmethod render :list-with-attr-names [data]
  (render-list data true))

(defmethod render :object [data]
  (let [hint (:shashurup.quf/hint (meta data))
        [names rndrs get-key] (parse-hint hint)]
    (render-obj data names rndrs false get-key)))

(defmethod render :object-attr [data]
  (let [hint (:shashurup.quf/hint (meta data))
        [_ rndrs _] (desc/table-desc (second hint) [(nth hint 2)])]
    ((first rndrs) data)))

(defonce startup-dummy
  (swap! output-handlers assoc :progress update-progress))
