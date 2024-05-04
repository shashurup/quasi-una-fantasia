(ns shashurup.quf.keymap)

(defonce keymap (atom {:base {"Enter" :eval-cell
                              "C-Enter" :eval-cell-and-stay
                              "Tab" :attempt-complete
                              "C-delete" :delete-cell
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

(defn- keymap-fns []
  (apply concat
         (for [m [(ns-interns 'shashurup.quf.core)
                  (ns-interns 'shashurup.quf.assistant)
                  (ns-interns 'shashurup.quf.editor)
                  (ns-interns 'shashurup.quf.vars)]]
           (->> m
                vals
                (filter #(:keymap/key (meta %)))))))

(defn register-fns! []
  (reset! fn-map (into {} (->> (keymap-fns)
                               (map #(vector (:keymap/key (meta %)) %))))))
