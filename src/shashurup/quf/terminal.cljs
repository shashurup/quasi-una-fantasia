(ns shashurup.quf.terminal
  (:require [clojure.math :refer [ceil round]]
            [clojure.string :refer [join]]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [goog.events :as gevents]
            [shashurup.quf.nrepl :as nrepl]
            [shashurup.quf.render :refer [eval-reply-handler]]
            [shashurup.quf.utils :as u]
            [xterm]))

(def font ["Monospace" 13])
(def measure-element (let [style (str "font-family: " (first font) "; "
                                      "font-size: " (second font))]
                       [:span.xterm-char-measure-element {:style style}
                        (join (repeat 32 "W"))]))

;; (defn cell-size []
;;   (let [canvas (js/OffscreenCanvas. 100 100)
;;         context (.getContext canvas "2d")]
;;     (set! (.-font context) (str (second font ) "px " (first font)))
;;     (let [metrics (.measureText context "W")]
;;       [(.-width metrics)
;;        (+ (.-fontBoundingBoxAscent metrics)
;;           (.-fontBoundingBoxDescent metrics))])))

;; A bit of a dirty hack to calculate terminal dimensions
;; While terminal object is not created
(defn get-cell-size []
  (let [el (crate/html measure-element)
        dpr (.-devicePixelRatio js/window)]
    (.appendChild (.-body js/document) el)
    (let [rect (.getBoundingClientRect el)
          width (.-width rect)
          height (.-height rect)]
      (.remove el)
      [(/ width 32)
       (-> height
           ceil
           (* dpr)
           ceil
           (/ dpr))])))

(def cell-size (get-cell-size))

(defn terminal-dimensions []
  (let [[w h] cell-size
        W (.-clientWidth (gdom/getElement "app"))
        H (* .75 (.-innerHeight js/window))]
    [(quot W w) (quot H h)]))

(defn send-terminal-dimensions []
  (nrepl/send-update-vars [['*term-dimensions* nil (terminal-dimensions)]]))

(defn get-out-element [id]
  (gdom/getElement (str "out-" id)))

(defn send-stdin [subj]
  (nrepl/send-op {:op "stdin"
                  :stdin subj}
                 nil))

(defn handle-key [subj]
  (send-stdin (.-key subj)))

(defn handle-resize [_]
  (send-terminal-dimensions)
  (send-stdin "\u001b[0m"))

(defonce terminals (atom {}))

(defn plug-terminal [id]
  (let [el (get-out-element id)
        [cols rows] (terminal-dimensions)
        terminal (xterm/Terminal. #js {:convertEol true
                                       :fontFamily (first font)
                                       :fontSize (second font)})]
    (swap! terminals assoc id terminal)
    (.resize terminal cols rows)
    (gevents/listen js/window
                    "resize"
                    (fn [_]
                      (let [[cols rows] (terminal-dimensions)]
                        (.resize terminal cols rows))))
    (.open terminal el)
    (.onKey terminal handle-key)
    (.focus terminal)))

(defn deactivate-terminal [id]
  (swap! terminals dissoc id))

(defn wrap-terminal-handler [handler]
  (fn [id {:keys [out err status] :as reply}]
    (if-let [terminal (get @terminals id)]
      (cond 
        (or out err) (.write terminal (or out err))
        (nrepl/terminated? status) (do (deactivate-terminal id)
                                       (handler id reply))
        :else (handler id reply))
      (let [data (nrepl/try-read-value-with-meta out)]
        (if (= (:shashurup.quf/hint (meta data)) :terminal)
          (plug-terminal id)
          (handler id reply))))))

(defonce startup-dummy 
  (do
    (swap! eval-reply-handler wrap-terminal-handler)
    (gevents/listen js/window "resize" handle-resize)
    (send-terminal-dimensions)
    (u/add-style-ref "css/xterm.css")))
