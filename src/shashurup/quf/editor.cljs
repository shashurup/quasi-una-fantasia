(ns shashurup.quf.editor
  (:require
   [clojure.string :as s]
   [crate.core :as crate]
   [shashurup.quf.markup :as markup]
   [shashurup.quf.utils :as u]
   [goog.dom :as gdom]
   [goog.dom.classlist :as gcls]))

(def ^:private pairs {"[" "]"
                      "(" ")"
                      "{" "}"
                      "\"" "\""})

(def ^:private closing (set (vals pairs)))

(defn get-input-element [id]
  (gdom/getElement (str "expr-" id)))

(defn get-selection [] (js/getSelection))

(defn get-range-0 [selection]
  (.getRangeAt selection 0))

(defn range-includes? [range node] (.intersectsNode range node))

(defn get-common-ancestor [selection]
  (.-commonAncestorContainer (get-range-0 selection)))

(defn collapsed? [selection] (.-isCollapsed selection))

(defn get-anchor-node [selection] (.-anchorNode selection))

(defn get-anchor-offset [selection] (.-anchorOffset selection))

(defn at-the-end?
  ([node offset] (>= offset (count (.-textContent node))))
  ([selection]
   (when-let [node (get-anchor-node selection)]
     (at-the-end? node (get-anchor-offset selection)))))

(defn get-focus-node [selection] (.-focusNode selection))

(defn get-focus-offset [selection] (.-focusOffset selection))

(defn make-text-node [text] (.createTextNode js/document text))

(defn parent-element [node] (.-parentElement node))

(defn text-content [node]
  (.-textContent node))

(defn insert-text-at-caret [text]
  (when-let [orig-sel (get-selection)]
    (.deleteFromDocument orig-sel)
    (let [sel (get-selection)
          node (get-anchor-node sel)
          offset (get-anchor-offset sel)
          node-text (.-textContent node)]
      (gdom/setTextContent node
                           (str (subs node-text 0 offset)
                                text
                                (subs node-text offset)))
      (.setStart (get-range-0 (get-selection)) node offset))))

(defn root? [node]
  (gcls/contains node "quf-input"))

(defn text-node? [node]
  (when node 
    (= (.-nodeType node) 3)))

(defn atom? [node]
  (and node
       (= (.-nodeName node) "SPAN")
       (not (gcls/contains node "quf-paren"))
       (not (gcls/contains node "quf-container"))))

(defn element? [node]
  (and node
       (= (.-nodeName node) "SPAN")
       (not (gcls/contains node "quf-paren"))))

(defn sexp? [node]
  (and (element? node)
       (gcls/contains node "quf-container")))

(defn in-atom? [node]
  (when node
    (atom? (parent-element node))))

(defn paren? [node]
  (and node
       (= (.-nodeName node) "SPAN")
       (gcls/contains node "quf-paren")))

(defn open-paren? [node]
  (and (paren? node) (get pairs (s/trim (.-textContent node)))))

(defn closing-paren? [node]
  (and (paren? node) (not (open-paren? node))))

(defn whitespace? [node]
  (and node
       (text-node? node)
       (not (paren? (parent-element node)))
       (not (in-atom? node))))

(defn parent-nodes [node]
  (take-while #(not (root? %))
              (iterate parent-element node)))

(defn root-node [node]
  (->> node
       (iterate parent-element)
       (take-while identity)
       (filter root?)
       first))

(defn previous-sibling [node] (.-previousSibling node))

(defn next-sibling [node] (.-nextSibling node))

(defn siblings [node f]
  (rest (take-while identity (iterate f node))))

(defn nodes-before [node] (siblings node previous-sibling))

(defn nodes-after [node] (siblings node next-sibling))

(defn nodes-between [begin end]
  (->> (nodes-after begin)
       (take-while #(not (identical? % end)))))

(defn sibling-elements-before [node]
  (filter element?
          (nodes-before node)))

(defn sibling-elements-after [node]
  (filter element?
          (nodes-after node)))

(defn next-sibling-element [node]
  (first (sibling-elements-after node)))

(defn prev-sibling-element [node]
  (first (sibling-elements-before node)))

(defn right-edge-of? [sel]
  (when (collapsed? sel)
    (let [node (get-anchor-node sel)
          offset (get-anchor-offset sel)]
      (when (empty? (s/trim (subs (.-textContent node) offset)))
        (empty? (->> node
                     nodes-after
                     (map text-content)
                     (map s/trim)
                     (reduce str)))))))

(defn left-edge-of? [sel]
  (when (collapsed? sel)
    (let [node (get-anchor-node sel)
          offset (get-anchor-offset sel)]
      (when (empty? (s/trim (subs (.-textContent node) 0 offset)))
        (empty? (->> node
                     nodes-before
                     (map text-content)
                     (map s/trim)
                     (reduce str)))))))

(defn right-edge-of-sexp? [sel]
  (and (closing-paren? (parent-element (get-anchor-node sel)))
       (right-edge-of? sel)))

(defn left-edge-of-sexp? [sel]
  (and (open-paren? (parent-element (get-anchor-node sel)))
       (left-edge-of? sel)))

(defn right-edge-of-atom? [sel]
  (and (in-atom? (get-anchor-node sel))
       (right-edge-of? sel)))

(defn left-edge-of-atom? [sel]
  (and (in-atom? (get-anchor-node sel))
       (left-edge-of? sel)))

(defn node-seq [subj]
  (tree-seq #(.hasChildNodes %) #(.-childNodes %) subj))

(defn text-node-seq [subj]
  (->> subj
       node-seq
       (filter text-node?)))

(defn prev-leaf-node [node]
  (when-let [prev (->> (parent-nodes node)
                       (map previous-sibling)
                       (remove nil?)
                       first)]
    (->> (iterate #(.-lastChild %) prev)
         (take-while identity)
         last)))

(defn next-leaf-node [node]
  (when-let [next (->> (parent-nodes node)
                       (map next-sibling)
                       (remove nil?)
                       first)]
    (->> (iterate #(.-firstChild %) next)
         (take-while identity)
         last)))

(defn next-to-caret [sel]
  (let [node (get-anchor-node sel)
        offset (get-anchor-offset sel)]
    (let [text (subs (text-content node) offset)]
      (if (empty? text)
        (when-let [node (->> (iterate next-leaf-node node)
                             rest
                             (take-while identity)
                             (drop-while #(empty? (text-content %)))
                             first)]
          [(text-content node) node])
        [text node]))))

(defn prev-to-caret [sel]
  (let [node (get-anchor-node sel)
        offset (get-anchor-offset sel)]
    (let [text (subs (text-content node) 0 offset)]
      (when (not-empty text)
        (last text)))))

(defn children [node] (seq (.-childNodes node)))

(defn sibling-elements-before-caret [sel]
  (let [node (get-anchor-node sel)
        parent (parent-element node)
        base (cond
               (left-edge-of-atom? sel) (prev-leaf-node node)
               (left-edge-of-sexp? sel) (prev-leaf-node node)
               (right-edge-of-sexp? sel) (parent-element parent)
               (atom? parent) parent
               (paren? parent) parent
               :else node)]
    (->> base
         (iterate previous-sibling)
         (take-while identity)
         (filter element?))))

(defn enclosing-sexp [sel]
  (let [node (get-common-ancestor sel)
        which (if (or (right-edge-of-sexp? sel)
                      (left-edge-of-sexp? sel))
                second
                first)]
    (->> node
         parent-nodes
         (filter sexp?)
         which)))

(defn set-position!
  ([selection node] (set-position! selection node 0))
  ([selection node offset]
   (.setPosition selection node offset)
   selection))

;; Looks a bit overcomplicated
;; however recreating ranges allows
;; to get rid of side effect such as
;; when you move the selection visually
;; it extends rater than move
(defn select! [sel subj]
  (let [range (.createRange js/document)]
    (if (vector? subj)
      (let [[node begin len] subj]
        (doto range
          (.setStart node begin)
          (.setEnd node (+ begin len))))
      (doto range
        (.collapse true)
        (.selectNode subj)))
    (doto sel
      (.removeAllRanges)
      (.addRange range))))

(defn select-element-by-text! [selection node]
  (.selectNode (get-range-0 selection)
               (parent-element node)))

;; sexp mode

(defn- chars-around [node offset]
  (let [text (.-wholeText node)]
    (cond (= offset 0) [nil (nth text offset)]
          (= offset (count text)) [(nth text (dec offset)) nil]
          :else [(nth text (dec offset)) (nth text offset)])))

(defn- alphanum? [subj]
  (when subj
    (or
     (.test (js/RegExp. "\\p{L}" "u") subj)
     (.test (js/RegExp. "\\p{Nd}" "u") subj))))

(defn- split-to-words [subj]
  (->> (js/RegExp. "[\\p{L}\\p{Nd}]+" "gu")
       (.matchAll subj)
       (map #(vector (.-index %) (first %)))))

(defn- node-at-offset [parent offset]
  (let [after-last (= offset (.-length parent))]
    (.item (.-childNodes parent)
           (if after-last (dec offset) offset))))

(defn- sexp-selection-kind [node offset]
  (if (text-node? node)
    (cond
      (in-atom? node) (cond (= offset 0) :atom-text-begin
                            (at-the-end? node offset) :atom-text-end
                            :else :atom-text-middle)
      (open-paren? (parent-element node)) (if (= offset 0)
                                            :open-text-begin
                                            :open-text-end)
      (closing-paren? (parent-element node)) (if (= offset 0)
                                               :close-text-begin
                                               :close-text-end)
      (whitespace? node) (cond (= offset 0) :whitespace-text-begin
                               (at-the-end? node offset) :whitespace-text-end
                               :else :whitespace-text-middle))
    (let [after-last (= offset (.-length node))
          node (.item (.-childNodes node)
                      (if after-last (dec offset) offset))]
      (cond (atom? node) (if after-last :atom-end :atom-begin)
            (open-paren? node) :open-begin
            (closing-paren? node) (if after-last :close-end :close-begin)
            (element? node) (if after-last :container-end :container-begin)
            (whitespace? node) (if after-last :whitespace-end :whitespace-begin)))))

(defn- sexp-adjust-selection []
  (let [sel (get-selection)]
    (when (or (collapsed? sel)
              (not (identical? (get-anchor-node sel)
                               (get-focus-node sel))))
      (let [anchor (get-anchor-node sel)
            offset (get-anchor-offset sel)
            kind (sexp-selection-kind anchor offset)]
        (cond
          (= :atom-text-middle kind) (.setEnd (get-range-0 sel)
                                              anchor
                                              (inc offset))
          (#{:atom-text-begin
             :atom-text-end} kind) (select-element-by-text! sel anchor)
          (= :whitespace-text-begin kind) (select!
                                           sel (or (prev-sibling-element anchor)
                                                   (next-sibling-element anchor)
                                                   (parent-element anchor)))
          (#{:whitespace-begin
             :whitespace-end} kind) (let [node (node-at-offset anchor offset)]
                                      (select!
                                       sel (or (prev-sibling-element node)
                                               (next-sibling-element node)
                                               (parent-element node))))
          (#{:whitespace-text-middle
             :whitespace-text-end} kind) (select!
                                          sel (or (next-sibling-element anchor)
                                                  (prev-sibling-element anchor)
                                                  (parent-element anchor)))
          (#{:open-text-begin
             :close-text-end} kind) (select!
                                     sel (parent-element
                                          (parent-element anchor)))
          (= :open-text-end kind) (let [paren (parent-element anchor)]
                                    (select!
                                     sel (or (next-sibling-element paren)
                                             (parent-element paren))))
          (= :close-text-begin kind) (let [paren (parent-element anchor)]
                                       (select!
                                        sel (or (prev-sibling-element paren)
                                                (parent-element paren))))
          (#{:atom-begin
             :atom-end
             :container-begin
             :container-end} kind) (select!
                                    sel (node-at-offset anchor offset))
          (#{:open-begin
             :close-end} kind) (select! sel anchor)
          (= :close-begin kind) (select!
                                 sel (or (prev-sibling-element
                                          (node-at-offset anchor offset))
                                         anchor)))))))

(defn- sexp-selection-level [sel]
  (let [anchor (get-anchor-node sel)]
    (cond (or (sexp? anchor)
              (root? anchor)) :element
          (text-node? anchor) (let [a-offset (get-anchor-offset sel)
                                    focus (get-focus-node sel)
                                    f-offset (get-focus-offset sel)
                                    [ap an] (chars-around anchor a-offset)
                                    [fp fn] (chars-around focus f-offset)]
                                (if (and (not (alphanum? ap))
                                         (alphanum? an)
                                         (alphanum? fp)
                                         (not (alphanum? fn)))
                                  :word
                                  :char)))))

(defn sexp-mode? [id]
  (gcls/contains (get-input-element id) "quf-sexp-mode"))

(defn sexp-mode
  "Turn on sexp mode - vim like mode for input expressions editing."
  {:keymap/key :sexp-mode}
  [id]
  (gcls/add (get-input-element id) "quf-sexp-mode")
  (sexp-adjust-selection))

(defn insert-mode
  "Return back into insert mode."
  {:keymap/key :insert-mode}
  [id]
  (.collapseToStart (get-selection))
  (gcls/remove (get-input-element id) "quf-sexp-mode"))

(defn append
  "Return back into insert mode at the end of the selection."
  {:keymap/key :append}
  [id]
  (.collapseToEnd (get-selection))
  (gcls/remove (get-input-element id) "quf-sexp-mode"))

(declare restructure)

(defn wrap [id open]
  (let [close (get pairs open)
        sel (get-selection)
        anchor (get-anchor-node sel)
        start (if (text-node? anchor)
                (parent-element anchor)
                (node-at-offset anchor (get-anchor-offset sel)))
        end (cond
              (collapsed? sel) start
              (text-node? anchor) (parent-element anchor)
              :else (node-at-offset anchor (dec (get-focus-offset sel))))
        open-node (make-text-node (str open " "))]
    (.insertBefore (parent-element start)
                   open-node
                   start)
    (.insertBefore (parent-element end)
                   (make-text-node close)
                   (next-sibling end))
    (set-position! sel open-node 1)
    (restructure (get-input-element id))
    (insert-mode id)))

(defn wrap-with-a-paren
  "Wrap current selection with parens."
  {:keymap/key :wrap-with-a-paren}
  [id]
  (wrap id "("))

(defn wrap-with-a-bracket
  "Wrap current selection with square brackets."
  {:keymap/key :wrap-with-a-bracket}
  [id]
  (wrap id "["))

(defn wrap-with-a-brace
  "Wrap current selection with curly braces."
  {:keymap/key :wrap-with-a-brace}
  [id]
  (wrap id "{"))

(defn wrap-with-quotes
  "Wrap current selection with double quotes."
  {:keymap/key :wrap-with-quotes}
  [id]
  (wrap id "\""))

(defn unwrap
  "Unwrap nearest enclosing container."
  {:keymap/key :unwrap}
  [id]
  (when-let [sel (get-selection)]
    (when-let [sexp (enclosing-sexp sel)]
      (->> (children sexp)
           (filter paren?)
           vec ;; makes a copy of node to delete
           (map #(.removeChild sexp %))
           doall)
      (restructure (get-input-element id)))))

(defn raise-sexp
  "Deletes siblings before and after the selection and unwraps."
  {:keymap/key :raise-sexp}
  [id]
  (let [sel (get-selection)
        anchor (get-anchor-node sel)
        start (if (text-node? anchor)
                (parent-element anchor)
                (node-at-offset anchor (get-anchor-offset sel)))
        end (cond
              (collapsed? sel) start
              (text-node? anchor) (parent-element anchor)
              :else (node-at-offset anchor (dec (get-focus-offset sel))))]
    (->> (concat (nodes-before start)
                 (nodes-after end))
         vec ;; makes a copy of nodes to delete
         (map #(.removeChild (parent-element start) %))
         doall)))

(defonce clipboard (atom ""))

(defn change
  "Change selected text - vim terminology - delete and turn insert mode."
  {:keymap/key :change-selection}
  [id]
  (when-let [sel (js/getSelection)]
    (.deleteFromDocument sel))
  (insert-mode id))

(defn delete
  "Delete selected text."
  {:keymap/key :delete-selection}
  [id]
  (when-let [sel (js/getSelection)]
    (reset! clipboard (.toString sel))
    (.deleteFromDocument sel)
    (sexp-adjust-selection)))

(defn yank
  "Yanks the selected text into local clipboard."
  {:keymap/key :yank}
  [id]
  (when-let [sel (js/getSelection)]
    (reset! clipboard (.toString sel))))

(defn- paste [id after?]
  (let [input (get-input-element id)
        sel (get-selection)
        text (if (text-node? (get-anchor-node sel))
               @clipboard
               (str (when after? " ")
                    @clipboard
                    (when-not after? " ")))]
    (if after?
      (.collapseToEnd sel)
      (.collapseToStart sel))
    (.insertNode (get-range-0 sel)
                 (make-text-node text))
    (.normalize input)
    (restructure input)
    (sexp-adjust-selection)))

(defn paste-before
  "Pastes local clipboard content."
  {:keymap/key :paste}
  [id]
  (paste id false))

(defn paste-after
  "Pastes local clipboard content after the selection."
  {:keymap/key :paste-after}
  [id]
  (paste id true))

(defn next-segment [sel]
  (let [anchor (get-anchor-node sel)
        f-offset (get-focus-offset sel)]
    (condp = (sexp-selection-level sel)
      :element (let [subj (node-at-offset anchor f-offset)]
                 (if (element? subj)
                   subj
                   (next-sibling-element subj)))
      :word (when-let [[p w] (->> (.-textContent anchor)
                                  split-to-words
                                  (drop-while #(< (first %) f-offset))
                                  first)]
              [anchor p (count w)])
      :char (when-not (at-the-end? anchor f-offset)
              [anchor f-offset 1]))))

(defn first-segment [sel]
  (let [anchor (get-anchor-node sel)
        offset (get-anchor-offset sel)]
    (condp = (sexp-selection-level sel)
      :element (first (filter element? (children anchor)))
      :word (when-let [[p w] (->> (.-textContent anchor)
                                  split-to-words
                                  first)]
              [anchor p (count w)])
      :char [anchor 0 1])))

(defn prev-segment [sel]
  (let [anchor (get-anchor-node sel)
        offset (get-anchor-offset sel)]
    (condp = (sexp-selection-level sel)
      :element (let [subj (node-at-offset anchor offset)]
                 (prev-sibling-element subj))
      :word (when-let [[p w] (->> (.-textContent anchor)
                                  split-to-words
                                  (take-while #(< (first %) offset))
                                  last)]
              [anchor p (count w)])
      :char (when (> offset 0)
              [anchor (dec offset) 1]))))

(defn last-segment [sel]
  (let [anchor (get-anchor-node sel)
        offset (get-anchor-offset sel)]
    (condp = (sexp-selection-level sel)
      :element (last (filter element? (children anchor)))
      :word (when-let [[p w] (->> (.-textContent anchor)
                                  split-to-words
                                  last)]
              [anchor p (count w)])
      :char (when-not (at-the-end? anchor (inc offset))
              (let [len (count (.-textContent anchor))]
                [anchor (dec len) 1])))))

(defn parent-segment [sel]
  (let [anchor (get-anchor-node sel)]
    (condp = (sexp-selection-level sel)
      :char (let [offset (get-anchor-offset sel)]
              (when-let [[p w] (->> (.-textContent anchor)
                                    split-to-words
                                    (take-while #(<= (first %) offset))
                                    last)]
                [anchor p (count w)]))
      :word (parent-element anchor)
      :element anchor)))

(defn top-segment [sel]
  (let [anchor (get-anchor-node sel)]
    (last (parent-nodes anchor))))

(defn first-child-segment [sel]
  (let [anchor (get-anchor-node sel)
        offset (get-anchor-offset sel)]
    (condp = (sexp-selection-level sel)
      :word [anchor offset 1]
      :element (let [node (node-at-offset anchor offset)]
                 (cond
                   (sexp? node)  (->> (children node)
                                      (filter element?)
                                      first)
                   (atom? node) (when-let [txt (first (children node))]
                                  (if-let [[p w] (->> (.-textContent txt)
                                                      split-to-words
                                                      first)]
                                    [txt p (count w)]
                                    [txt 0 1]))))
      nil)))

(defn first-character [sel]
  (let [anchor (get-anchor-node sel)
        offset (get-anchor-offset sel)
        node (node-at-offset anchor offset)]
    (when-let [txt (->> (text-node-seq node)
                        (filter #(element? (parent-element %)))
                        first)]
      [txt 0 1])))

(defn move-forward
  "Move current selection forward."
  {:keymap/key :move-forward}
  []
  (let [sel (get-selection)]
    (when-let [seg (next-segment sel)]
      (select! sel seg))))

(defn move-last
  "Move current selection to the end."
  {:keymap/key :move-last}
  []
  (let [sel (get-selection)]
    (when-let [seg (last-segment sel)]
      (select! sel seg))))

(defn move-backward
  "Move current selection backward."
  {:keymap/key :move-back}
  []
  (let [sel (get-selection)]
    (when-let [seg (prev-segment sel)]
      (select! sel seg))))

(defn move-first
  "Move current selection to the beginning."
  {:keymap/key :move-first}
  []
  (let [sel (get-selection)]
    (when-let [seg (first-segment sel)]
      (select! sel seg))))

(defn move-up
  "Select parent element."
  {:keymap/key :move-up}
  []
  (let [sel (get-selection)]
    (when-let [seg (parent-segment sel)]
      (select! sel seg))))

(defn move-top
  "Select topmost element."
  {:keymap/key :move-top}
  []
  (let [sel (get-selection)]
    (when-let [seg (top-segment sel)]
      (select! sel seg))))

(defn move-down
  "Select first child element."
  {:keymap/key :move-down}
  []
  (let [sel (get-selection)]
    (when-let [seg (first-child-segment sel)]
      (select! sel seg))))

(defn move-bottom
  "Select first character."
  {:keymap/key :move-bottom}
  []
  (let [sel (get-selection)]
    (when-let [seg (first-character sel)]
      (select! sel seg))))

(defn extend-selection
  "Extends current selection forward."
  {:keymap/key :extend-selection}
  [id]
  (let [sel (get-selection)]
    (when-let [seg (next-segment sel)]
      (if (vector? seg)
        (let [[node begin len] seg]
          (.extend sel node (+ begin len)))
        (.setEndAfter (get-range-0 sel) seg)))))


;; auto pairs
;; todo rework this somehow
;; it doesn't make sense when you have to move over a closing element

(defn- handle-pairs [e]
  (let [ch (.-data e)
        tp (.-inputType e)]
    (when (= tp "insertText")
      (when-let [pair (get pairs ch)]
        (insert-text-at-caret pair)))))

(defn- handle-pair-key [e]
  (let [ch (.-key e)]
    (when (closing ch)
      (let [sel (get-selection)
            [text _] (next-to-caret sel)]
        (when (= ch (first text))
          (.preventDefault e)
          (.modify sel "move" "forward" "character"))))))

;; input structure

(def control-syms #{"if" "if-not" "if-let"
                    "when" "when-not" "when-let"
                    "def" "defn" "defn-" "defonce"
                    "cond" "condp"
                    "let" "binding" "letfn"
                    "for" "do" "doseq" "doall"
                    "fn" "recur" "loop"
                    "->" "->>"
                    "require" "ns"})

(def leaf-class-map {:string "quf-string"
                     :char "quf-char"
                     :number "quf-number"
                     :keyword "quf-keyword"
                     :client-var "quf-client-var"
                     :symbol "quf-symbol"
                     :bool "quf-bool"
                     :nil "quf-nil"
                     :open "quf-paren"
                     :close "quf-paren"})

(defn figure-class [type value]
  (let [class (leaf-class-map type)]
    (if (control-syms value)
      (str class " quf-control")
      class)))

(defn replace-content [el new-content]
  (.replaceChildren el)
  (doseq [child new-content]
    (.append el (if (string? child)
                  child
                  (crate/html child)))))

(defn structure->html [structure]
  (for [[value type] structure]
    (condp = type
      :whitespace value
      :container [:span.quf-container (structure->html value)]
      [:span {:class (figure-class type value)
              :data-quf-type (name type)} value])))

(defn skeleton [subj]
  (if (coll? subj)
    ;; skeleton for markup
    (for [[value type] subj]
      (if (= type :container)
        (skeleton value)
        type)
      )
    ;; skeleton for for the input expression
    (for [child (.-childNodes subj)]
      (cond
        (text-node? child) :whitespace
        (.hasAttribute child "data-quf-type")
            (keyword (.getAttribute child "data-quf-type"))
        :else (skeleton child)))))

(defn get-node-text-offset [node parent]
  (->> parent
       text-node-seq
       (take-while #(not (identical? % node)))
       (map #(.-length %))
       (reduce +)))

(defn get-caret-position [el]
  (let [sel (get-selection)
        anchor (get-anchor-node sel)
        offset (get-anchor-offset sel)
        txt (if (text-node? anchor)
              anchor
              (->> (node-at-offset anchor offset)
                   text-node-seq
                   first))
        pos (if (text-node? anchor) offset 0)]
    (+ (get-node-text-offset txt el) pos)))

(defn set-caret-position! [el pos]
  (when-let [[start _ node]
             (->> el
                  text-node-seq
                  (reductions (fn [[start end cur] node]
                                [end (+ end (.-length node)) node])
                              [0 0 nil])
                  rest
                  (filter (fn [[_ end _]] (<= pos end)))
                  first)]
    (set-position! (get-selection) node (- pos start))))

(defn restructure [el]
  (let [text (.-textContent el)
        markup (markup/parse text)]
    (when (not= (skeleton markup) (skeleton el))
      (let [pos (get-caret-position el)]
        (replace-content el (structure->html markup))
        (set-caret-position! el pos)))))

;; autoident

(defn indent-reference [before sexp]
  (let [open (first (filter open-paren? (.-childNodes sexp)))
        call? (= (.-textContent open) "(")
        atoms (reverse before)
        atom-count (count atoms)]
    (if (= 0 atom-count)
      [open 3]
      (if call?
        (if (> atom-count 1)
          [(second atoms) 0]
          [(first atoms) 2])
        [(first atoms) 0]))))

(defn indent-column [node]
  (let [[head tail] (->> (iterate prev-leaf-node node)
                         (rest)
                         (take-while identity)
                         (map #(.-textContent %))
                         (split-with #(not (s/includes? % "\n"))))]
    (+ (apply + (map count head))
       (when tail
         (count (last (s/split-lines (first tail))))))))

(defn indent []
  (let [sel (get-selection)
        node (get-anchor-node sel)
        before (sibling-elements-before-caret sel)]
    (when-let [sexp (enclosing-sexp sel)]
      (let [[ref-node offset] (indent-reference before sexp)
            column (indent-column ref-node)
            pos (get-anchor-offset sel)]
        (.insertData node pos (.repeat " " (+ column offset)))
        (set-position! sel node (+ pos column offset))))))

(defn need-indent? [e]
  (or (= (.-inputType e) "insertLineBreak")
      (and (= (.-inputType e) "insertText")
           (nil? (.-data e)))))

;; Paren highlight

(defn clear-highlight [subj]
  (doseq [p (->> subj
                 node-seq
                 (filter paren?))]
    (gcls/remove p  "quf-highlight")))

(defn highlight-parens [_]
  (let [sel (get-selection)]
    (when-let [root (root-node (get-anchor-node sel))]
      (clear-highlight root)
      (let [[text node] (next-to-caret sel)
            ch (first text)]
        (when (or (pairs ch)
                  (closing ch))
          (when-let [sexp (->> node
                               parent-nodes
                               (filter sexp?)
                               first)]
            (doseq [p (filter paren? (children sexp))]
              (gcls/add p "quf-highlight"))))))))

(defn blur-parens [e]
  (clear-highlight (.-target e)))

(.addEventListener js/document "selectionchange" highlight-parens)

;; paste text without formatting

(defn- handle-paste [e]
  (.preventDefault e)
  (let [text (.getData (.-clipboardData e) "text/plain")]
    (.execCommand js/document "insertText" false text)))

(defn- handle-input-change [e]
  (handle-pairs e)
  (when (need-indent? e)
    (indent))
  (restructure (.-target e)))

(defn plug [input]
  (.addEventListener input "input" handle-input-change)
  (.addEventListener input "keydown" handle-pair-key)
  (.addEventListener input "blur" blur-parens)
  (restructure input)
  (.addEventListener input "paste" handle-paste))
