(ns shashurup.quf.render
  (:require [clojure.string :as s]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [shashurup.quf.desc :as desc]
            [shashurup.quf.utils :as u]))

;; Tree

(defonce check-id (atom 0))

(defn new-check-id []
  (swap! check-id inc))

(defn make-scalar [class val]
  [:span {:class class} (pr-str val)])

(defmulti render (fn [subj]
                   (if-let [hint (:shashurup.quf/hint (meta subj))]
                     (if (keyword? hint)
                       hint
                       (first hint))
                     (type subj))))

(defmethod render :default [subj]
  [:span.ra (pr-str subj)])

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

(defn make-composite [subj cont-type render-fn]
  (let [check-id (new-check-id)
        [prefix suffix] (get paren-map cont-type)]
    [:div.quf-composite-wrapper
     [:label {:for check-id} prefix]
     [:input {:id check-id
              :type "checkbox"
              :style "display: none"}]
     [:div {:class (str "quf-composite-body-"
                        (subs (str cont-type) 1))}
      (for [node subj] (render-fn node))]
     [:label.quf-ellipsis {:for check-id} "\u2026"] ;; ellipsis
     [:span.quf-closing-paren suffix]]))

(defmethod render PersistentVector [subj]
  (make-composite subj :vector render))

(defmethod render List [subj]
  (make-composite subj :list render))

(defmethod render PersistentHashSet [subj]
  (make-composite subj :set render))

(defn render-map-entry [[k v]]
  [:div.quf-map-entry (render k) (render v)])

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
  [:span {:class "quf-tag"} (str "#" tag " " arg)])


;; Table

(defmulti render-cell type)

(defmethod render-cell :default [value]
  [:td.ra (if (coll? value)
            (if (and (= :tag (:shashurup.quf/hint (meta value)))
                     (= 'object (first value)) )
              (last (second value))
              (render value))
            (pr-str value))])

(defmethod render-cell nil [_] [:td.ra])

(defmethod render-cell js/String [value]
  [:td {:class (str "ra " "quf-string-cell")} value])

(defmethod render-cell js/Number [value]
  [:td {:class (str "ra " "quf-number-cell")}
   (.format (js/Intl.NumberFormat.) value)])

(defmethod render-cell js/Date [value]
  [:td {:class (str "ra " "quf-date-cell")}
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
  (let [header (crate/html [:th.ra name])]
    (.addEventListener header "click" header-click)
    header))

(defmethod render :table [data]
  (let [hint (:shashurup.quf/hint (meta data))
        [names rndrs] (if (keyword? hint)
                        (desc/table-desc (guess-columns data))
                        (when (coll? hint)
                          (let [sec (second hint)]
                            (if (keyword? sec)
                              (desc/table-desc sec (nth hint 2))
                              (desc/table-desc sec)))))]
    [:table.ra [:thead [:tr (for [name names]
                              (render-header name))]]
               [:tbody (for [row data]
                         [:tr (for [rndr rndrs]
                                (render-cell (rndr row)))])]]))
