(ns shashurup.quf.keymap
  (:require [cljs.tools.reader :refer [read-string]]
            [clojure.string :as s]
            [shashurup.quf.assistant :as assistant]
            [shashurup.quf.editor :as editor]
            [shashurup.quf.render :refer [render]]
            [shashurup.quf.utils :as u]))

(def default-keymap {:base {"Enter" :eval-cell
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
                            "C-p" :search-history
                            "C-h" :toggle-doc
                            "C-s" :cycle-result-height
                            "C-." :load-ns-dialog
                            "A-C-t" :new-tab
                            "C-m" :show-checkboxes
                            "C-e" :expand-client-vars
                            "C-[" :sexp-mode}
                     :completions {"C-j" :use-next-candidate
                                   "C-k" :use-prev-candidate
                                   "Escape" :hide-completion-candidates}
                     :sexp-mode {"i" :insert-mode
                                 "d" :delete-selection
                                 "c" :change-selection
                                 "a" :append
                                 "y" :yank
                                 "p" :paste
                                 "S-p" :paste-after
                                 "h" :move-back
                                 "S-h" :move-first
                                 "l" :move-forward
                                 "S-l" :move-last
                                 "j" :move-down
                                 "S-j" :move-bottom
                                 "k" :move-up
                                 "S-k" :move-top
                                 "v" :extend-selection
                                 "S-9" :wrap-with-a-paren
                                 "[" :wrap-with-a-bracket
                                 "S-[" :wrap-with-a-brace
                                 "S-'" :wrap-with-quotes
                                 "u" :unwrap
                                 "r" :raise-sexp}})

(defonce symbol-codes {"=" "Equal"
                       "[" "BracketLeft"
                       "]" "BracketRight"
                       ";" "Semicolon"
                       "'" "Quote"
                       "`" "BackQuote"
                       "\\" "Backslash"
                       "," "Comma"
                       "." "Period"
                       "/" "Slash"})

(defn- canonize [subj]
  (let [parts (s/split (s/trim subj) #"-")
        code (last parts)
        mods (butlast parts)]
    (let [mods (vec (sort mods))
          code (if (> (count code) 1)
                 code
                 (cond
                   (re-matches #"[a-zA-Z]" code) (str "key" code)
                   (re-matches #"[0-9]" code) (str "digit" code)
                   :else (get symbol-codes code code)))]
      (->> (conj mods code)
           (s/join "-")
           s/lower-case))))

(defn- canonize-keymap [subj]
  (-> subj
      (update :base update-keys canonize)
      (update :completions update-keys canonize)
      (update :sexp-mode update-keys canonize)))

(defonce keymap (atom default-keymap))

(defonce keymap-canon (atom (canonize-keymap default-keymap)))

(add-watch keymap
           :update-canon
           (fn [_ _ _ new]
             (reset! keymap-canon (canonize-keymap new))))

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
  (str (when (.-altKey e) "a-")
       (when (.-ctrlKey e) "c-")
       (when (.-shiftKey e) "s-")
       (s/lower-case (or (.-code e) ""))))

(defn- handler-fn [mode key]
  (when-let [fn-key (get-in @keymap-canon [mode key])]
    (get @fn-map fn-key)))

(defn- find-handler [id key]
  (or (when (editor/sexp-mode? id)
        (handler-fn :sexp-mode key))
      (when (assistant/active id)
        (handler-fn :completions key))
      (handler-fn :base key)
      (when (editor/sexp-mode? id)
        identity)))

(defn keydown-handler-for
  ([id]
   (fn [e]
     (when-let [f (find-handler id (key-event->str e))]
       (. js/console debug "found a local handler")
       (f id)
       (.stopPropagation e)
       (.preventDefault e))))
  ([]
   (fn [e]
     (when-let [f (handler-fn :base (key-event->str e))]
       (when (:keymap/global (meta f))
         (. js/console debug "found a global handler")
         (f)
         (.preventDefault e))))))

(def item-name "keymap")

(defn merge-keymap! [subj]
  (u/store-item item-name
                (swap! keymap
                       #(merge-with merge % subj))))

(defn replace-keymap! [subj]
  (u/store-item item-name (reset! keymap subj)))

(def mode-names {:base "Basic"
                 :completions "Completions mode"
                 :sexp-mode "Sexp mode"})

(defn- render-mode [keymap mode]
  [:div
   [:h3 (mode-names mode)]
   [:table.quf
    [:tr.quf [:th.quf "Key"] [:th.quf "Function"] [:th.quf "Description"]]
    (for [[key fn-key] (sort (get keymap mode))]
      [:tr.quf
       [:td.quf.quf-string key]
       [:td.quf.quf-keyword fn-key]
       [:td.quf (:doc (meta (@fn-map fn-key)))]])]])

(defmethod render :keymap [subj]
  (let [merge-fn (u/gen-js-call #(merge-keymap! subj))
        replace-fn (u/gen-js-call #(replace-keymap! subj))]
    [:div
     (for [[mode _] subj]
       (render-mode subj mode))
     [:button.quf {:onclick merge-fn} "Merge"]
     [:button.quf {:onclick replace-fn} "Replace"]]))

(defn init [ns-maps]
  (register-fns! ns-maps)
  (when-let [km (u/load-item item-name)]
    (replace-keymap! km)))
