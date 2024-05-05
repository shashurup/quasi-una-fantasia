(ns shashurup.quf.keymap
  (:require [shashurup.quf.assistant :as assistant]
            [shashurup.quf.editor :as editor]
            [shashurup.quf.render :refer [render]]))

(defonce keymap (atom {:base {"Enter" :eval-cell
                              "C-Enter" :eval-cell-and-stay
                              "Tab" :attempt-complete
                              "C-Delete" :delete-cell
                              "C-u" :delete-cell
                              "A-u" :hide-input
                              "A-C-l" :delete-all
                              "C-i" :insert-cell-above
                              "C-o" :insert-cell-below
                              "C-y" :copy-cell-with-expr
                              "A-y" :append-cell-with-expr
                              "C-j" :cell-below
                              "C-k" :cell-above
                              "C-r" :search-history
                              "C-h" :toggle-doc
                              "C-s" :cycle-result-height
                              "C-=" :load-ns-dialog
                              "C-t" :new-tab
                              "C-m" :show-checkboxes
                              "C-e" :expand-client-vars
                              "C-;" :sexp-mode}
                       :completions {"C-j" :use-next-candidate
                                     "C-k" :use-prev-candidate
                                     "Escape" :hide-completion-candidates}
                       :sexp-mode {"i" :insert-mode
                                   "d" :delete-selection
                                   "c" :change-selection
                                   "h" :move-back
                                   "l" :move-forward
                                   "j" :move-down
                                   "k" :move-up
                                   "w" :next-element-begin
                                   "e" :next-element-end
                                   "b" :prev-element
                                   "v" :extend-selection
                                   "S-(" :wrap-with-a-paren
                                   "[" :wrap-with-a-bracket
                                   "S-{" :wrap-with-a-brace
                                   "S-\"" :wrap-with-quotes
                                   "u" :unwrap
                                   "f" :forward-slurp
                                   "S-F" :forward-barf
                                   "a" :backward-slurp
                                   "S-A" :backward-barf
                                   "S-^" :move-start
                                   "S-$" :move-end
                                   "0" :move-start}}))

(defonce fn-map (atom {}))

(defn- keymap-fns [ns-maps]
  (apply concat
         (for [m ns-maps]
           (->> m
                vals
                (filter #(:keymap/key (meta %)))))))

(defn register-fns! [ns-maps]
  (reset! fn-map (into {} (->> (keymap-fns ns-maps)
                               (map #(vector (:keymap/key (meta %)) %))))))

(defn- key-event->str [e]
  (str (when (.-altKey e) "A-")
       (when (.-ctrlKey e) "C-")
       (when (.-shiftKey e) "S-")
       (.-key e)))

(defn- handler-fn [mode key]
  (when-let [fn-key (get-in @keymap [mode key])]
    (get @fn-map fn-key)))

(defn- find-handler [id key]
  (.log js/console "Searching " key)
  (or (when (editor/sexp-mode? id)
        (handler-fn :sexp-mode key))
      (when (assistant/active id)
        (handler-fn :completions key))
      (handler-fn :base key)
      (when (editor/sexp-mode? id)
        identity)))

(defn keydown-handler-for [id]
  (fn [e]
    (when-let [f (find-handler id (key-event->str e))]
      (.log js/console "found")
      (f id)
      (.preventDefault e))))

(defn- render-mode [mode title]
  [:div
   [:h3 title]
   [:table.quf
    [:tr.quf [:th.quf "Key"] [:th.quf "Function"] [:th.quf "Description"]]
    (for [[key fn-key] (get @keymap mode)]
      [:tr.quf
       [:td.quf.quf-string key]
       [:td.quf.quf-keyword fn-key]
       [:td.quf (:doc (meta (@fn-map fn-key)))]])]])

(defmethod render :keymap [subj]
  [:div
   (render-mode :base "Basic")
   (render-mode :completions "Completions mode")
   (render-mode :sexp-mode "Sexp mode")])
