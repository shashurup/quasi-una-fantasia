(ns shashurup.quf.vars
  (:require
   [clojure.string :as s]
   [shashurup.quf.editor :as editor]
   [shashurup.quf.keymap :as keymap]
   [shashurup.quf.storage :as storage]
   [goog.dom :as gdom]))

(defonce server-vars (atom {}))

(defn read-pending-updates! []
  (let [[val _] (reset-vals! server-vars {})]
    (when (not-empty val)
      {:var-updates
       (pr-str (for [[k v] val] [k nil v]))})))

(defn push-server-updates!
  ;; TODO three args version
  ;; ([sym fn val] ...)
  ([sym val] (swap! server-vars assoc sym val)))

(defn get-input-element [id]
  (gdom/getElement (str "expr-" id)))

(defn nearest-cell-selection
  "Checked items from the nearest upper cell with checkboxes"
  [input-node]
  (->> (.-parentElement input-node)
       (iterate #(.-previousElementSibling %))
       (take-while identity)
       rest
       (filter #(gdom/getElementByClass "quf-check" %))
       first
       (gdom/getElementsByClass "quf-check")
       (filter #(.-checked %))
       (map #(.-value %))
       vec))

(defn all-cells-selection
  "All checked items"
  [_]
  (->> (gdom/getElementsByClass "quf-check")
       (filter #(.-checked %))
       (map #(.-value %))
       vec))

(defn all-cell-exprs
  "Vector of all cell expressions"
  [_]
  (->> (gdom/getElementsByClass "quf-input")
       (map #(.-textContent %))
       vec))

(defn stored-namespaces
  "Namespaces stored in browser's local storage"
  [_] (storage/stored-nses))

(defn active-keymap
  "Currently active keymap"
  [_] @keymap/keymap)

(def client-vars {"s" nearest-cell-selection
                  "s-all" all-cells-selection
                  "cells" all-cell-exprs
                  "stored" stored-namespaces
                  "keymap" (fn [_] @keymap/keymap)})

(defn replace-var [text node]
  (let [var (s/trim text)]
    (if-let [f (and (s/starts-with? var "$$")
                    (get client-vars (subs var 2)))]
      (s/replace-first text var (f node))
      text)))

(defn replace-client-vars [node]
  (->> node
       editor/text-node-seq
       (map #(.-textContent %))
       (map #(replace-var % node))
       (apply str)))

(defn expand-client-vars
  "Replace all the client vars in the input with their values."
  {:keymap/key :expand-client-vars}
  [id]
  (let [node (get-input-element id)]
    (doseq [text-node (editor/text-node-seq node)]
      (.replaceWith text-node
                    (replace-var (.-textContent text-node)
                                 node)))
    (editor/restructure node)))
