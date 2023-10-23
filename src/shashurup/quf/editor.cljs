(ns shashurup.quf.editor
  (:require
   [goog.dom :as gdom]
   [goog.dom.classlist :as gcls]))

(defn get-input-element [id]
  (gdom/getElement (str "expr-" id)))

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
