(ns rackushka.editor
  (:require
   [goog.dom :as gdom]))

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

(defn- handle-input-change [e]
  (handle-pairs e))

(defn plug [input]
  (.addEventListener input "input" handle-input-change))
