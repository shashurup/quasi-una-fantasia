(ns shashurup.quf.editor
  (:require
   [goog.dom :as gdom]
   [goog.dom.classlist :as gcls]))

(defn get-input-element [id]
  (gdom/getElement (str "expr-" id)))

(defn get-selection [] (js/getSelection))

(defn collapsed? [selection] (.-isCollapsed selection))

(defn get-anchor-node [selection] (.-anchorNode selection))

(defn get-anchor-offset [selection] (.-anchorOffset selection))

(defn get-focus-node [selection] (.-focusNode selection))

(defn get-focus-offset [selection] (.-focusOffset selection))

(defn previous-sibling [node] (.-previousSibling node))

(defn next-sibling [node] (.-nextSibling node))

(defn siblings [node f]
  (rest (take-while identity (iterate f node))))

(defn nodes-before [node] (siblings node #(.-previousSibling %)))

(defn nodes-after [node] (siblings node #(.-nextSibling %)))

(defn sexp-element? [node]
  (and node (= (.-nodeName node) "SPAN")))

(defn string-sexp-element? [node]
  (and (sexp-element? node)
       (gcls/contains node "quf-string")))

(defn text-node? [node]
  (= (.-nodeType node) 3))

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
  (if-let [text (.-firstChild node)]
    (if (text-node? text) text node)
    node))

(defn whole-node-selected? [sel]
  (let [node (get-anchor-node sel)
        start (get-anchor-offset sel)
        end (get-focus-offset sel)]
    (= (abs (- end start))
       (count (.-textContent node)))))

(defn string-interior-selected? [sel]
  (= (sort [(get-anchor-offset sel) (get-focus-offset sel)])
     [1 (dec (count (.-textContent (get-anchor-node sel))))]))

(defn set-position! [selection node offset] (.setPosition selection node offset))

(defn select-whole-element! [selection node]
  (let [parent (.-parentElement node)
        range (.getRangeAt selection 0)]
    (.selectNode range parent)))

(defn select-string-interior! [selection node]
  (.setBaseAndExtent selection node 1 node (dec (count (.-textContent node)))))

(defn container-at-cursor []
  (.-startContainer (.getRangeAt (get-selection) 0)))

(defn cursor-node-and-offset []
  (let [range (.getRangeAt (get-selection) 0)]
    [(.-startContainer range)
     (.-startOffset range)]))

(defn insert-text-at-cursor [text]
  (let [[node offset] (cursor-node-and-offset)
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
          (set-position! sel (first-text-node-or-self node) 0)
          (when-let [node (prev-sexp-element node)]
            (set-position! sel (first-text-node-or-self node) 0)))))))

(defn next-element [id]
  (when-let [sel (get-selection)]
    (when (collapsed? sel)
      (let [node (parent-sexp-element-if-any sel)]
        (when-let [node (next-sexp-element node)]
          (set-position! sel (first-text-node-or-self node) 0))))))

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

(defn selection-state [sel]
  (or (intra-element-selection-state sel)
      nil ;; todo figure sexp-interior
      ))

(defn extend-selection [id]
  (when-let [sel (js/getSelection)]
    (condp = (selection-state sel)
      :in-element      (select-whole-element! sel (get-anchor-node sel))
      :in-string       (select-string-interior! sel (get-anchor-node sel))
      :in-sexp         true ;; select-sexp-interior
      :sexp-interior   true ;; select-sexp
      true)))

(defn change [id]
 (when-let [sel (js/getSelection)]
   (.deleteFromDocument sel))
 (insert-mode id))

(defn delete [id]
  (when-let [sel (js/getSelection)]
    (.deleteFromDocument sel)))

;; basics

(defn- handle-input-change [e]
  (handle-pairs e))

;; paste text without formatting
(defn- handle-paste [e]
  (.preventDefault e)
  (let [text (.getData (.-clipboardData e) "text/plain")]
    (.execCommand js/document "insertText" false text)))

(defn plug [input]
  (.addEventListener input "input" handle-input-change)
  (.addEventListener input "paste" handle-paste))
