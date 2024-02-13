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

(defn get-input-element [id]
  (gdom/getElement (str "expr-" id)))

(defn get-selection [] (js/getSelection))

(defn get-range-0 [selection]
  (.getRangeAt selection 0))

(defn get-the-only-range []
  (when-let [selection (get-selection)]
    (.getRangeAt selection 0)))

(defn range-includes? [range node] (.intersectsNode range node))

(defn get-common-ancestor [selection]
  (.-commonAncestorContainer (get-range-0 selection)))

(defn collapsed? [selection] (.-isCollapsed selection))

(defn get-anchor-node [selection] (.-anchorNode selection))

(defn get-anchor-offset [selection] (.-anchorOffset selection))

(defn at-the-end? [selection]
  (when-let [node (get-anchor-node selection)]
    (= (count (.-textContent node))
       (get-anchor-offset selection))))

(defn get-focus-node [selection] (.-focusNode selection))

(defn get-focus-offset [selection] (.-focusOffset selection))

(defn make-text-node [text] (.createTextNode js/document text))

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
    (atom? (.-parentElement node))))

(defn paren? [node]
  (and node
       (= (.-nodeName node) "SPAN")
       (gcls/contains node "quf-paren")))

(defn whitespace? [node]
  (and node
       (text-node? node)
       (not (paren? (.-parentElement node)))
       (not (in-atom? node))))

(defn atom-of-type? [node type]
  (and (atom? node)
       (gcls/contains node type)))

(defn end-of-whitespace? [node offset]
  (and (whitespace? node)
       (= (.-length node) offset)))

(defn string-atom? [node] (atom-of-type? node "quf-string"))

(defn get-start-element [selection]
  (let [range (get-range-0 selection)
        start (.-startContainer range)
        parent (.-parentElement start)
        offset (.-startOffset range)]
    (if (text-node? start)
      (cond
        (paren? parent) (.-parentElement parent)
        (atom? parent) parent
        :else start)
      (.item (.-childNodes start) offset))))

(defn get-end-element [selection]
  (let [range (get-range-0 selection)
        end (.-endContainer range)
        parent (.-parentElement end)
        offset (.-endOffset range)]
    (if (text-node? end)
      (if (atom? parent) parent end)
      (.item (.-childNodes end) (dec offset)))))

(defn parent-nodes [node]
  (take-while #(not (root? %))
              (iterate #(.-parentElement %) node)))

(defn previous-sibling [node] (.-previousSibling node))

(defn next-sibling [node] (.-nextSibling node))

(defn siblings [node f]
  (rest (take-while identity (iterate f node))))

(defn nodes-before [node] (siblings node #(.-previousSibling %)))

(defn nodes-after [node] (siblings node #(.-nextSibling %)))

(defn nodes-between [begin end]
  (->> (nodes-after begin)
       (take-while #(not (identical? % end)))))

(defn sibling-elements-before [node]
  (filter element?
          (nodes-before node)))

(defn sibling-elements-after [node]
  (filter element?
          (nodes-after node)))

(defn text-node-seq [subj]
  (->> subj
       (tree-seq #(.hasChildNodes %)
                 #(.-childNodes %))
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

(defn prev-atom-text [node]
  (->> (iterate prev-leaf-node node)
       (take-while identity)
       rest
       (filter in-atom?)
       first))

(defn next-atom-text [node]
  (->> (iterate next-leaf-node node)
       (take-while identity)
       rest
       (filter in-atom?)
       first))

(defn children [node] (seq (.-childNodes node)))

(defn enclosing-sexp [sel]
  (->> (get-common-ancestor sel)
       parent-nodes
       (filter sexp?)
       first))

(defn first-text-node-or-self [node]
  (when node
    (if-let [text (.-firstChild node)]
      (if (text-node? text) text node)
      node)))

(defn whole-node-selected? [sel]
  (let [node (get-anchor-node sel)
        start (get-anchor-offset sel)
        end (get-focus-offset sel)]
    (= (abs (- end start))
       (count (.-textContent node)))))

(defn string-interior-range [text]
  [(inc (or (s/index-of text "\"") 0))
   (or (s/last-index-of text "\"")
       (count text))])

(defn string-interior-selected? [sel]
  (= (sort [(get-anchor-offset sel) (get-focus-offset sel)])
     (string-interior-range (.-textContent (get-anchor-node sel)))))

(defn set-position!
  ([selection node] (set-position! selection node 0))
  ([selection node offset]
   (.setPosition selection node offset)
   selection))

(defn set-position-at-end! [selection node]
  (set-position! selection
                 node
                 (if (text-node? node)
                   (count (.-textContent node))
                   (.-length (.-childNodes node)))))

(defn select-whole-atom! [selection node]
  (let [parent (.-parentElement node)
        range (get-range-0 selection)]
    (.selectNode range parent)))

(defn select-string-interior! [selection node]
  (let [text (.-textContent node)
        [begin end] (string-interior-range text)]
    (.setBaseAndExtent selection node begin node end)))

(defn select-whole-sexp! [selection node]
  (.selectNode (get-range-0 selection) node))

(defn select-sexp-interior! [selection node]
  (let [first-child (.-firstChild node)
        last-child (.-lastChild node)]
    (cond (root? node)
              (doto (get-range-0 selection)
                (.setStartBefore first-child)
                (.setEndAfter last-child))

          (and (paren? first-child)
               (paren? last-child))
              (doto (get-range-0 selection)
                (.setStartAfter first-child)
                (.setEndBefore last-child))

          :else (select-whole-sexp! selection node))))

;; sexp mode

(defn sexp-mode? [id]
  (gcls/contains (get-input-element id) "quf-sexp-mode"))

(defn sexp-mode [id]
  (gcls/add (get-input-element id) "quf-sexp-mode"))

(defn insert-mode [id]
  (gcls/remove (get-input-element id) "quf-sexp-mode"))

(defn prev-element [id]
  (when-let [sel (get-selection)]
    (if (collapsed? sel)
      ;; move cursor backwards
      (let [node (get-anchor-node sel)]
        (if (> (get-anchor-offset sel) 0)
          (set-position! sel node)
          (when-let [node (prev-atom-text node)]
            (set-position! sel node))))
      ;; extend selection backwards
      (let [start (get-start-element sel)]
        (when-let [node (first (sibling-elements-before start))]
          (.setStartBefore (get-range-0 sel) node))))))

(defn next-element [id sel-fn]
  (when-let [sel (get-selection)]
    (if (collapsed? sel)
      ;; move cursor backwards
      (let [node (get-anchor-node sel)]
        (when-let [node (next-atom-text node)]
          (sel-fn sel node)))
      ;; extend selection backwards
      (let [end (get-end-element sel)]
        (when-let [node (first (sibling-elements-after end))]
          (.setEndAfter (get-range-0 sel) node))))))

(defn next-element-begin [id]
  (next-element id set-position!))

(defn next-element-end [id]
  (next-element id set-position-at-end!))

(defn move-forward [id]
  (when-let [sel (get-selection)]
    (.modify sel "move" "forward" "character")))

(defn move-back [id]
  (when-let [sel (get-selection)]
    (.modify sel "move" "backward" "character")))

(defn move-up [id]
  (when-let [sel (get-selection)]
    (.modify sel "move" "backward" "line")))

(defn move-down [id]
  (when-let [sel (get-selection)]
    (.modify sel "move" "forward" "line")))

(defn move-start [id]
  (when-let [sel (get-selection)]
    (set-position! sel (first (text-node-seq (get-input-element id))))))

(defn move-end [id]
  (when-let [sel (get-selection)]
    (.setEndAfter (get-range-0 sel)
                  (last (text-node-seq (get-input-element id))))
    (.collapseToEnd sel)))

(defn intra-atom-selection-state [sel]
  (when (identical? (get-anchor-node sel)
                    (get-focus-node sel))
    (let [anchor-node (get-anchor-node sel)
          parent (.-parentElement anchor-node)]
      (when (atom? parent)
        (when (not (whole-node-selected? sel))
          (if (string-atom? parent)
            (if (string-interior-selected? sel)
              :in-atom
              :in-string)
            :in-atom))))) )

(defn sexp-selection-state [sel]
  (let [common-ancestor (get-common-ancestor sel)]
    (let [first-child (.-firstChild common-ancestor)
          last-child (.-lastChild common-ancestor)
          range (get-range-0 sel)]
      (if (and (paren? first-child)
               (paren? last-child)
               (not (range-includes? range first-child))
               (not (range-includes? range last-child))
               (every? #(range-includes? range %)
                       (nodes-between first-child last-child)))
        :sexp-interior
        :in-sexp))))

(defn selection-state [sel]
  (or (intra-atom-selection-state sel)
      (sexp-selection-state sel)))

(defn fix-selection!
  "Move selection to the more appropriate place.
  For instance, when it is at the end of a whitespace
  move it to the next element, etc."
  [selection]
  (when (and (collapsed? selection)
             (at-the-end? selection))
    (let [node (get-anchor-node selection)
          parent (.-parentElement node)]
      (when (or (whitespace? node) (paren? parent))
        (set-position! selection (next-leaf-node node))))))

(defn find-container-node [sel]
  (let [node (get-common-ancestor sel)]
    (->> (iterate #(.-parentElement %) node)
         (remove text-node?)
         (remove paren?)
         first)))

(defn extend-selection [id]
  (fix-selection! (get-selection))
  (when-let [sel (get-selection)]
    (condp = (selection-state sel)
      :in-atom         (select-whole-atom! sel (get-anchor-node sel))
      :in-string       (select-string-interior! sel (get-anchor-node sel))
      :in-sexp         (select-sexp-interior! sel (find-container-node sel))
      :sexp-interior   (select-whole-sexp! sel (find-container-node sel))
      true)))

(declare restructure)

(defn wrap [id open]
  (fix-selection! (get-selection))
  (let [close (get pairs open)
        sel (get-selection)
        start (get-start-element sel)
        end (if (collapsed? sel) start (get-end-element sel))
        open-node (make-text-node (str open " "))]
    (.insertBefore (.-parentElement start)
                   open-node
                   start)
    (.insertBefore (.-parentElement end)
                   (make-text-node close)
                   (next-sibling end))
    (set-position! sel open-node 1)
    (restructure (get-input-element id))))

(defn wrap-with-a-paren [id]
  (wrap id "("))

(defn wrap-with-a-bracket [id]
  (wrap id "["))

(defn wrap-with-a-brace [id]
  (wrap id "{"))

(defn wrap-with-quotes [id]
  (wrap id "\""))

(defn unwrap [id]
  (when-let [sel (get-selection)]
    (when-let [sexp (enclosing-sexp sel)]
      (->> (children sexp)
           (filter paren?)
           vec ;; makes a copy of node to delete
           (map #(.removeChild sexp %))
           doall)
      (restructure (get-input-element id)))))

(defn forward-slurp [id]
  (when-let [sel (get-selection)]
    (when-let [sexp (enclosing-sexp sel)]
      (->> (nodes-after sexp)
           (u/take-until element?)
           (map #(.insertBefore sexp % (.-lastChild sexp)))
           doall))))

(defn backward-slurp [id]
  (when-let [sel (get-selection)]
    (when-let [sexp (enclosing-sexp sel)]
      (->> (nodes-before sexp)
           (u/take-until element?)
           (map #(.insertBefore sexp % (.-nextSibling (.-firstChild sexp))))
           doall))) )

(defn forward-barf [id]
  (when-let [sel (get-selection)]
    (when-let [sexp (enclosing-sexp sel)]
      (->> (nodes-before (.-lastChild sexp))
           (u/take-until whitespace?)
           (map #(.insertBefore (.-parentElement sexp)
                                %
                                (next-sibling sexp)))
           doall))))

(defn backward-barf [id]
  (when-let [sel (get-selection)]
    (when-let [sexp (enclosing-sexp sel)]
      (->> (nodes-after (.-firstChild sexp))
           (u/take-until whitespace?)
           (map #(.insertBefore (.-parentElement sexp) % sexp))
           doall))))

(defn change [id]
 (when-let [sel (js/getSelection)]
   (.deleteFromDocument sel))
 (insert-mode id))

(defn delete [id]
  (when-let [sel (js/getSelection)]
    (.deleteFromDocument sel)))

;; auto pairs
;; todo rework this somehow
;; it doesn't make sense when you have to move over a closing element

(defn insert-text-at-cursor [text]
  (let [sel (get-selection)
        node (get-anchor-node sel)
        offset (get-anchor-offset sel)
        node-text (.-textContent node)]
    (gdom/setTextContent node
                         (str (subs node-text 0 offset)
                              text
                              (subs node-text offset)))
    (.setStart (.getRangeAt (get-selection) 0) node offset)))

(defn- handle-pairs [e]
  (let [ch (.-data e)
        tp (.-inputType e)]
    (when (= tp "insertText")
      (when-let [pair (get pairs ch)]
        (insert-text-at-cursor pair)))))

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

(defn get-cursor-position [el]
  (let [range (.getRangeAt (js/getSelection) 0)
        start-el (.-startContainer range)
        start-pos (.-startOffset range)]
    (+ (get-node-text-offset start-el el) start-pos)))

(defn set-cursor-position! [el pos]
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
  (.log js/console "Checking structure ...")
  (let [text (.-textContent el)
        markup (markup/parse text)]
    (when (not= (skeleton markup) (skeleton el))
      (.log js/console "Restructure!")
      (let [pos (get-cursor-position el)]
        (replace-content el (structure->html markup))
        (set-cursor-position! el pos)))))

;; autoident

(defn indent-reference [anchor-node sexp]
  (let [open (first (filter paren? (.-childNodes sexp)))
        call? (= (.-textContent open) "(")
        atoms (reverse (sibling-elements-before anchor-node))
        atom-count (count atoms)]
    (if (= 0 atom-count)
      [open 3]
      (if call?
        (if (> atom-count 1)
          [(second atoms) 0]
          [(first atoms) 2])
        [(first atoms) 0]))))

(defn indent-column [node]
  (let [[head & tail] (->>  (iterate prev-leaf-node node)
                            (rest)
                            (take-while identity)
                            (map #(.-textContent %))
                            (partition-by #(s/includes? % "\n")))]
    (.log js/console (pr-str head) (pr-str tail))
    (+ (apply + (map count head))
       (when tail
         (count (last (s/split-lines (ffirst tail))))))))

(defn indent []
  (let [sel (get-selection)
        node (get-anchor-node sel)]
    (when-let [sexp (->> (parent-nodes node)
                         (filter sexp?)
                         first)]
      (let [[ref-node offset] (indent-reference node sexp)
            column (indent-column ref-node)
            pos (get-anchor-offset sel)]
        (.log js/console
              "ref-node" (.-textContent ref-node)
              "column " column
              "offset " offset)
        (.insertData node pos (.repeat " " (+ column offset)))
        (set-position! sel node (+ pos column offset))))))

(defn need-indent? [e]
  (or (= (.-inputType e) "insertLineBreak")
      (let [sel (get-selection)
            node (get-anchor-node sel)
            offset (get-anchor-offset sel)]
        (.log js/console "text is " (pr-str  (.-textContent node)) "offset is " offset)
        (and (= (.-inputType e) "insertText")
             (nil? (.-data e))))))

;; paste text without formatting

(defn- handle-paste [e]
  (.preventDefault e)
  (let [text (.getData (.-clipboardData e) "text/plain")]
    (.execCommand js/document "insertText" false text)))

(defn- handle-input-change [e]
  ;(handle-pairs e)
  (.log js/console "On input " (.-data e) (.-inputType e))
  (when (need-indent? e)
    (.log js/console "Indenting")
    (indent))
  (restructure (.-target e))
  )

(defn plug [input]
  (.addEventListener input "input" handle-input-change)
  (restructure input)
  (.addEventListener input "paste" handle-paste))
