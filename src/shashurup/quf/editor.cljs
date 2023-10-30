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

(defn previous-sibling [node] (.-previousSibling node))

(defn next-sibling [node] (.-nextSibling node))

(defn siblings [node f]
  (rest (take-while identity (iterate f node))))

(defn nodes-before [node] (siblings node #(.-previousSibling %)))

(defn nodes-after [node] (siblings node #(.-nextSibling %)))

(defn word-element? [node]
  (and node (= (.-nodeName node) "SPAN")))

(defn parent-word-element-if-any [selection]
  (when-let [node (get-anchor-node selection)]
    (if (word-element? (.-parentElement node))
      (.-parentElement node)
      node)))

(defn first-word-element [nodes]
  (first (filter word-element? nodes)))

(defn prev-word-element [node]
  (first-word-element (nodes-before node)))

(defn next-word-element [node]
  (first-word-element (nodes-after node)))

(defn set-position! [selection node offset] (.setPosition selection node offset))

(defn container-at-cursor []
  (.-startContainer (.getRangeAt (js/getSelection) 0)))

(defn cursor-node-and-offset []
  (let [range (.getRangeAt (js/getSelection) 0)]
    [(.-startContainer range)
     (.-startOffset range)]))

(defn insert-text-at-cursor [text]
  (let [[node offset] (cursor-node-and-offset)
        node-text (.-textContent node)]
    (gdom/setTextContent node
                         (str (subs node-text 0 offset)
                              text
                              (subs node-text offset)))
    (.setStart (.getRangeAt (js/getSelection) 0) node offset)))

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

(defn prev-word [id]
  (when-let [sel (get-selection)]
    (when (collapsed? sel)
      (let [node (parent-word-element-if-any sel)]
        (if (> (get-anchor-offset sel) 0)
          (set-position! sel node 0)
          (when-let [node (prev-word-element node)]
            (set-position! sel node 0)))))))

(defn next-word [id]
  (when-let [sel (get-selection)]
    (when (collapsed? sel)
      (let [node (parent-word-element-if-any sel)]
        (when-let [node (next-word-element node)]
          (set-position! sel node 0))))))

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
