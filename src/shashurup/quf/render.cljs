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

(defn hint-args [subj]
  (when-let [hint (:shashurup.quf/hint (meta subj))]
    (when (coll? hint)
      (let [sec (second hint)]
        (if (keyword? sec)
          [sec (nth hint 2)]
          [nil sec])))))

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

(defn more-items? [subj]
  (get-in (meta subj) [:shashurup.quf/range :more?]))

(defn push-context [parent child & args]
  (if (coll? child)
    (let [{path ::path
           expr ::expr
           {from :from} :shashurup.quf/range
           more :shashurup.quf/more} (meta parent)
          fix #(if (number? %) (+ from %) %)]
      (vary-meta child merge {::path (into (if more [] (or path []))
                                           (map fix args))
                              ::expr (or more expr)}))
    child))

(defn pop-context [subj]
  (vary-meta subj update ::path pop))

(defn load-more-ellipsis [f]
  [:span {:style "cursor: pointer"
          :onclick (u/gen-js-call f)}
   "\u2026"])

(defn more-handler [subj el-fn render-fn fragment-fn]
  (fn [e]
    (let [{path ::path
           expr ::expr
           {to :to
            more? :more?} :shashurup.quf/range
           more :shashurup.quf/more} (meta subj)
          target (el-fn (.-target e))
          parent (.-parentElement target)
          ins (fn [resp]
                (when (contains? resp :value)
                  (let [value (push-context subj (:value resp))
                        fragment (-> value
                                     render-fn
                                     crate/html
                                     fragment-fn)]
                    (.remove target)
                    (doseq [el (vec (.-children fragment))]
                      (.append parent el)))))]
      (nrepl/send-eval (or more expr)
                       ins
                       (if more
                         u/eval-extra
                         {:shashurup.quf/path (or path [])
                          :shashurup.quf/range {:from to
                                                :to (+ to u/quota)}})))))

(defn make-composite [subj cont-type render-fn]
  (let [check-id (new-check-id)
        [prefix suffix] (get paren-map cont-type)]
    [:div.quf-composite-wrapper.quf-container
     [:label {:for check-id} prefix]
     [:input {:id check-id
              :type "checkbox"
              :style "display: none"}]
     [:div {:class (str "quf-composite-body-"
                        (subs (str cont-type) 1))}
      (for [[idx item] (map-indexed vector subj)
            :let [item (push-context subj item idx)]]
        (render-fn item))
      (when (more-items? subj)
        (load-more-ellipsis
         (more-handler subj
                       identity
                       render
                       #(first (.getElementsByTagName % "div")))))]
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
  (let [[k v] entry
        v (push-context (pop-context entry) v k)]
      [:div.quf-map-entry
       (render k)
       (render v)]))

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

(defmethod render :sequence [subj]
  [:div (for [[idx item] (map-indexed vector subj)
              :let [item (push-context subj item idx)]]
          (render item))])

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

(defmethod render :table [data]
  (let [[type-key columns] (hint-args data)
        get-key (get-in @desc/object-types [type-key :key])
        col-desc (desc/column-descriptors type-key
                                          (or columns
                                              (guess-columns data)))]
    [:table.quf.quf-container
     [:thead [:tr
              (when get-key [:th.quf-check-cell])
              (for [[title _] col-desc]
                (render-header title))]]
     [:tbody (for [row data]
               [:tr
                (when get-key [:td.quf-check-cell (render-checkbox (get-key row))])
                (for [[_ rndr] col-desc]
                  (render-cell (rndr row)))])
      (when (more-items? data)
        [:tr
         [:td
          (load-more-ellipsis
           (more-handler data
                         #(.-parentElement (.-parentElement %))
                         render
                         #(first (.getElementsByTagName % "tbody"))))]])]]))

(defn render-obj [obj col-desc show-attr-names get-key]
  [:div.quf-object 
   (for [[title rndr] col-desc]
     (let [val (rndr obj)
           val-el (if (coll? val)
                    (render val)
                    [:span (str val)])]
       (if show-attr-names
         [:div.quf-map-entry [:span.quf-keyword title] "=" val-el]
         [:div.quf-object-attr val-el])))
   (when get-key (render-checkbox (get-key obj)))])

(defn render-list [data show-attr-names]
  (let [[type-key columns] (hint-args data)
        get-key (get-in @desc/object-types [type-key :key])
        col-desc (desc/column-descriptors type-key
                                          (or columns
                                              (guess-columns data)))]
    [:div.quf-composite-body-list.quf-container
     (for [obj data]
       (render-obj obj col-desc show-attr-names get-key))
     (when (more-items? data)
       (load-more-ellipsis
        (more-handler data
                      identity
                      render
                      identity)))]))

(defmethod render :list [data]
  (render-list data false))

(defmethod render :list-with-attr-names [data]
  (render-list data true))

(defmethod render :object [data]
  (let [[type-key columns] (hint-args data)
        get-key (get-in @desc/object-types [type-key :key])
        col-desc (desc/column-descriptors type-key
                                          (or columns
                                              (guess-columns data)))]
    (render-obj data col-desc false get-key)))

(defmethod render :object-attr [data]
  (let [[type-key attr] (hint-args data)
        [_ rndr] (first (desc/column-descriptors type-key [attr]))]
    (rndr data)))

;; Tree

(defonce cur-tree-id (atom 0))

(defn find-ellipsis [subj]
  (.-firstElementChild (.-lastElementChild (.-parentElement subj))))

(defn- call-once [subj attr]
  (fn [e]
    (.removeAttribute (.-target e) attr)
    (subj e)))

(defn tree-content-loader [tree-id action key]
  (let [code (concat action [key])]
    (fn [_]
      (let [target (gdom/getElement (str tree-id "-content"))
            insert (fn [resp]
                     (when (contains? resp :value)
                       (.replaceChildren
                        target
                        (-> resp
                            :value
                            (vary-meta assoc ::expr code)
                            render
                            crate/html))))]
        (nrepl/send-eval code insert u/eval-extra)))))

(defn render-tree-level [data [render-fn children-key
                               tree-id actions get-key :as params]]
  [:div.quf-tree
   (for [[idx item] (map-indexed vector data)]
     (let [id (str "tree-item-" (swap! cur-tree-id inc))
           b-id (str id "-b")
           r-id (str id "-r")
           action (:default (or (:shashurup.quf/actions (meta item))
                                actions))
           has-children (contains? item children-key)
           children (children-key item)
           more (and has-children
                     (empty? children)
                     (get-in (meta children) [:shashurup.quf/range :more?]))
           onselect (when get-key
                      (u/gen-js-call (tree-content-loader tree-id
                                                          action
                                                          (get-key item))))
           onchange (when more
                      (u/gen-js-call
                       (call-once
                        (more-handler children
                                      find-ellipsis
                                      #(render-tree-level % params)
                                      identity) "onchange")))]
       [:div.quf-tree-item {:aria-expanded "false"}
        (if has-children
          (list
           [:input.quf-tree-button (merge {:id b-id
                                           :type "checkbox"}
                                          (when onchange
                                            {:onchange onchange}))]
           [:label {:for b-id} ""])
          [:span.quf-tree-button-space])
        [:input.quf-tree-item (merge  {:id r-id
                                       :name tree-id
                                       :type "radio"}
                                      (when onselect
                                        {:onchange onselect}))]
        [:label.quf-tree-item {:for r-id} (render-fn item)]
        (when has-children
          (render-tree-level (push-context data children idx children-key)
                             params))]))
   (when (more-items? data)
     (load-more-ellipsis
      (more-handler data
                    identity
                    #(render-tree-level % params)
                    identity)))])

(defmethod render :tree [data]
  (let [[type-key params] (hint-args data)
        type-actions (get-in @desc/object-types [type-key :actions])
        type-key-fn (get-in @desc/object-types [type-key :key])
        {name-key :name
         children-key :children
         actions :actions
         get-key :key} (merge {:name :name
                               :children :children
                               :actions type-actions
                               :key (or type-key-fn :key)}
                              params)
        [_ render-fn] (first (desc/column-descriptors type-key [name-key]))
        tree-id (str "quf-tree-" (swap! cur-tree-id inc))]
    [:div.quf-tree-top
     [:div.quf-tree-left
      (render-tree-level data [render-fn children-key tree-id
                               actions get-key])]
     [:div.quf-tree-right {:id (str tree-id "-content")}]]))

(defonce startup-dummy
  (swap! output-handlers assoc :progress update-progress))
