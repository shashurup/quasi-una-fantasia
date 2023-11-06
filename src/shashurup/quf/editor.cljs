(ns shashurup.quf.editor
  (:require
   [crate.core :as crate]
   [shashurup.quf.markup :as markup]
   [goog.dom :as gdom]
   [goog.dom.classlist :as gcls]))

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

(defn previous-sibling [node] (.-previousSibling node))

(defn next-sibling [node] (.-nextSibling node))

(defn siblings [node f]
  (rest (take-while identity (iterate f node))))

(defn nodes-before [node] (siblings node #(.-previousSibling %)))

(defn nodes-after [node] (siblings node #(.-nextSibling %)))

(defn nodes-between [begin end]
  (->> (nodes-after begin)
       (take-while #(not (identical? % end)))))

(defn text-node? [node]
  (when node 
    (= (.-nodeType node) 3)))

(defn sexp-element? [node]
  (and node
       (= (.-nodeName node) "SPAN")
       (not (gcls/contains node "quf-paren"))
       (not (gcls/contains node "quf-container"))))

(defn paren? [node]
  (and node
       (= (.-nodeName node) "SPAN")
       (gcls/contains node "quf-paren")))

(defn whitespace? [node]
  (and node
       (text-node? node)
       (not (paren? (.-parentElement node)))
       (not (sexp-element? (.-parentElement node)))))

(defn sexp-element-of-type? [node type]
  (and (sexp-element? node)
       (gcls/contains node type)))

(defn string-sexp-element? [node] (sexp-element-of-type? node "quf-string"))

(defn text-node-seq [subj]
  (->> subj
       (tree-seq #(.hasChildNodes %)
                 #(.-childNodes %))
       (filter text-node?)))

(defn parent-sexp-element-if-any [selection]
  (when-let [node (get-anchor-node selection)]
    (if (sexp-element? (.-parentElement node))
      (.-parentElement node)
      node)))

(defn first-sexp-element [nodes]
  (first (filter sexp-element? nodes)))

(defn prev-sexp-element [node]
  (first-sexp-element (nodes-before node)))

(defn next-sexp-element [node]
  (first-sexp-element (nodes-after node)))

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

(defn string-interior-selected? [sel]
  (= (sort [(get-anchor-offset sel) (get-focus-offset sel)])
     [1 (dec (count (.-textContent (get-anchor-node sel))))]))

(defn set-position!
  ([selection node] (set-position! selection node 0))
  ([selection node offset]
   (.setPosition selection node offset)
   selection))

(defn select-whole-element! [selection node]
  (let [parent (.-parentElement node)
        range (get-range-0 selection)]
    (.selectNode range parent)))

(defn select-string-interior! [selection node]
  (.setBaseAndExtent selection node 1 node (dec (count (.-textContent node)))))

(defn select-whole-sexp! [selection node]
  (.selectNode (get-range-0 selection) node))

(defn select-sexp-interior! [selection node]
  (let [first-child (.-firstChild node)
        last-child (.-lastChild node)]
    (if (and (paren? first-child)
             (paren? last-child))
      (doto (get-range-0 selection)
            (.setStartAfter first-child)
            (.setEndBefore last-child))
      (select-whole-sexp! selection node))))

;; sexp mode

(defn sexp-mode? [id]
  (gcls/contains (get-input-element id) "quf-sexp-mode"))

(defn sexp-mode [id]
  (gcls/add (get-input-element id) "quf-sexp-mode"))

(defn insert-mode [id]
  (gcls/remove (get-input-element id) "quf-sexp-mode"))

(defn prev-element [id]
  (when-let [sel (get-selection)]
    (when (collapsed? sel)
      (let [node (parent-sexp-element-if-any sel)]
        (if (> (get-anchor-offset sel) 0)
          (set-position! sel (first-text-node-or-self node))
          (when-let [node (prev-sexp-element node)]
            (set-position! sel (first-text-node-or-self node))))))))

(defn next-element [id]
  (when-let [sel (get-selection)]
    (when (collapsed? sel)
      (let [node (parent-sexp-element-if-any sel)]
        (when-let [node (next-sexp-element node)]
          (set-position! sel (first-text-node-or-self node)))))))

(defn move-forward [id]
  (when-let [sel (get-selection)]
    (.modify sel "move" "forward" "character")))

(defn move-back [id]
  (when-let [sel (get-selection)]
    (.modify sel "move" "backward" "character")))

(defn intra-element-selection-state [sel]
  (when (identical? (get-anchor-node sel)
                    (get-focus-node sel))
    (let [anchor-node (get-anchor-node sel)
          parent (.-parentElement anchor-node)]
      (when (sexp-element? parent)
        (when (not (whole-node-selected? sel))
          (if (string-sexp-element? parent)
            (if (string-interior-selected? sel)
              :in-element
              :in-string)
            :in-element))))) )

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
  (or (intra-element-selection-state sel)
      (sexp-selection-state sel)))

(defn fix-selection!
  "Move selection to the more appropriate place.
  For instance, when it is at the end of a whitespace
  move it to the next element, etc."
  [selection]
  (when (and (collapsed? selection)
             (at-the-end? selection))
    (let [node (get-anchor-node selection)
          parent (.-parentElement node)
          next (or (next-sibling node)
                   (next-sibling parent))]
      (when (or (whitespace? node)
                (and (paren? parent)
                     (sexp-element? next)))
        (set-position! selection
                       (first-text-node-or-self next))))))

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
      :in-element      (select-whole-element! sel (get-anchor-node sel))
      :in-string       (select-string-interior! sel (get-anchor-node sel)) ;; todo handle regexp literals
      :in-sexp         (select-sexp-interior! sel (find-container-node sel))
      :sexp-interior   (select-whole-sexp! sel (find-container-node sel))
      true)))

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

(def ^:private pairs {"[" "]"
                      "(" ")"
                      "{" "}"
                      "\"" "\""})

(defn- handle-pairs [e]
  (let [ch (.-data e)
        tp (.-inputType e)]
    (when (= tp "insertText")
      (when-let [pair (get pairs ch)]
        (insert-text-at-cursor pair)))))

;; input structure

(def leaf-class-map {:string "quf-string"
                     :char "quf-char"
                     :number "quf-number"
                     :keyword "quf-keyword"
                     :symbol "quf-symbol"
                     :bool "quf-bool"
                     :nil "quf-nil"
                     :open "quf-paren"
                     :close "quf-paren"})

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
      [:span {:class (leaf-class-map type)
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

(defn restructure [e]
  (let [el (.-target e)
        text (.-textContent el)
        markup (markup/parse text)]
    (when (not= (skeleton markup) (skeleton el))
      (.log js/console "Restructure!")
      (let [pos (get-cursor-position el)]
        (replace-content el (structure->html markup))
        (set-cursor-position! el pos)))))

;; paste text without formatting

(defn- handle-paste [e]
  (.preventDefault e)
  (let [text (.getData (.-clipboardData e) "text/plain")]
    (.execCommand js/document "insertText" false text)))

(defn- handle-input-change [e]
  ;(handle-pairs e)
  (restructure e)
  )

(defn plug [input]
  (.addEventListener input "input" handle-input-change)
  (.addEventListener input "paste" handle-paste))
