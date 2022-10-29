(ns ^:figwheel-hooks rackushka.core
  (:use-macros
   [crate.def-macros :only [defpartial]])
  (:require
   [rackushka.desc :as desc]
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [cljs.reader :refer [read-string]]
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


(defn nrepl-op [op callback]
  (let [msg (pr-str op)
        req (js/XMLHttpRequest.)]
    (.open req "POST" "repl")
    (.setRequestHeader req "Content-Type" "application/octet-stream")
    (gevents/listen req
                    "loadend"
                    #(callback (if (= 200 (.-status req))
                                 (read-string (.-responseText req))
                                 (.-responseText req))))
    (.send req msg)))

(defn read-value [subj]
  (try
    (read-string subj)
    (catch js/Object _
      ^{:rackushka/hint :rackushka/text} [subj])))

(defn merge-eval-result [subj]
  (if (string? subj)
    {:err subj}
    {:id (:id (first subj))
     :out (->> subj
               (map #(select-keys % [:out :err :ex :root-ex]))
               (remove empty?))
     :value (->> subj
                 (map :value)
                 (remove nil?)
                 (map read-value))}))

(defn update-ns [resp]
  (when-let [ns (some :ns resp)]
    (swap! app-state assoc :ns ns)))

(defn history-append [expr]
  (when-not (empty? expr)
    (swap! app-state update :history #(conj % expr))))

(defn update-session [resp]
  (swap! app-state assoc :session (:new-session (first resp))))

(defn nrepl-eval [expr callback]
  (let [session (:session @app-state)]
    (if session
      (nrepl-op {:op "eval"
                 :code expr
                 :session session
                 :nrepl.middleware.print/print "rackushka.srv/pr-with-meta"}
                (fn [resp]
                  (history-append expr)
                  (update-ns resp)
                  (callback (merge-eval-result resp))))
      (nrepl-op {:op "clone"}
                (fn [resp]
                  (update-session resp)
                  (nrepl-eval expr callback))))))

;; Tree

(defn create-cell [id ns]
  (crate/html [:div {:id (str "cell-" id)}
               [:span.ra-prompt (str ns "=> ")]
               [:input {:id (str "expr-" id) :type "text" :size 80}]
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
  [:pre (pr-str subj)])

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

(defn make-composite [subj prefix suffix render-fn]
  (let [check-id (new-check-id)]
    [:div.ra-composite-wrapper
     [:label {:for check-id} prefix]
     [:input {:id check-id
              :type "checkbox"
              :style "display: none"}]
     [:div
      (for [node subj]
        [:div.ra-composite-entry (render-fn node)])]
     [:label.ra-ellipsis {:for check-id} "..."]
     [:span.ra-closing-paren suffix]]))

(defmethod render PersistentVector [subj]
  (make-composite subj "[" "]" render))

(defmethod render List [subj]
  (make-composite subj "(" ")" render))

(defmethod render PersistentHashSet [subj]
  (make-composite subj "#{" "}" render))

(defn render-map-entry [[k v]]
  (list (render k) (render v)))

(defn make-map [subj]
  (make-composite subj "{" "}" render-map-entry))

(defmethod render PersistentArrayMap [subj]
  (make-map subj))

(defmethod render PersistentHashMap [subj]
  (make-map subj))

(defmethod render :text [subj]
  [:pre (s/join "\n" subj)])

(defmethod render :html [subj] subj)

;; Table

(defmulti render-cell type)

(defmethod render-cell :default [value]
  [:td.ra (if (coll? value)
            (render value)
            (pr-str value))])

(defmethod render-cell nil [_] [:td.ra])

(defmethod render-cell js/String [value]
  [:td {:class (str "ra " "ra-string-cell")} value])

(defmethod render-cell js/Number [value]
  [:td {:class (str "ra " "ra-number-cell")}
   (.format (js/Intl.NumberFormat.) value)])

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

(defn focus-next-cell [id]
  (if-let [el (gdom/getElement (str "expr-" (inc id)))]
    (.focus el)
    (add-new-cell)))

(defn apply-result [id result]
  (let [valdiv (gdom/getElement (str "result-" id))
        outdiv (gdom/getElement (str "out-" id))]
    (gdom/removeChildren outdiv)
    (apply gdom/append
           outdiv
           (for [line (:out result)]
             (crate/html [:p {:class (out-class line)}
                          (s/join ", " (vals line))])))
    (gdom/removeChildren valdiv)
    (mapv (comp #(gdom/appendChild valdiv %)
                crate/html
                render)
          (:value result))
    (focus-next-cell id)))

(defn eval-expr [id]
  (let [expr (.-value (gdom/getElement (str "expr-" id)))]
    (nrepl-eval expr #(apply-result id %))))

(defn add-new-cell []
  (let [ns (:ns @app-state)]
    (if ns
      (let [id (new-cell-id)
            cell (create-cell id ns)
            keydown (fn [e]
                      (when (= e.code "Enter")
                        (eval-expr id)))]
        (gdom/appendChild (get-app-element) cell)
        (let [expr-input (gdom/getElement (str "expr-" id))]
          (.focus expr-input)
          (.addEventListener expr-input "keydown" keydown)))
      (nrepl-eval "*ns*" #(add-new-cell)))))

(gevents/listen js/window
                "load"
                (fn [_] (add-new-cell)))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
