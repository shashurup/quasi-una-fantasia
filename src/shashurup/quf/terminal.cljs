(ns shashurup.quf.terminal
  (:require [clojure.math :refer [ceil round]]
            [clojure.string :refer [join]]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [goog.events :as gevents]
            [shashurup.quf.nrepl :as nrepl]
            [shashurup.quf.render :refer [handle-extra-data
                                          handle-output
                                          set-type!]]
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

(defn handle-key [subj]
  (.log js/console subj)
  (nrepl/send-op {:op "stdin"
                  :stdin (.-key subj)}
                 nil))

(defmethod handle-extra-data :terminal [_ id]
  (let [el (get-out-element id)
        [cols rows] (terminal-dimensions)
        terminal (xterm/Terminal. #js {:convertEol true
                                       :fontFamily (first font)
                                       :fontSize (second font)})]
    (set-type! el :terminal)
    (set! (.-quf-terminal el) terminal)
    (.resize terminal cols rows)
    (.open terminal el)
    (.onKey terminal handle-key)
    (.focus terminal)))

(defmethod handle-output :terminal [target reply]
  (.write (.-quf-terminal target)
          (some reply [:out :err])))

(gevents/listen js/window "resize" send-terminal-dimensions)
(send-terminal-dimensions)
(u/add-style-ref "css/xterm.css")
