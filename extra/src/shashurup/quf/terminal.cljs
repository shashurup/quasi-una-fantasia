(ns shashurup.quf.terminal
  (:require [clojure.math :refer [ceil round]]
            [clojure.string :refer [join]]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [goog.events :as gevents]
            [shashurup.quf.nrepl :as nrepl]
            [shashurup.quf.render :refer [cell-handlers
                                          output-handlers]]
            [shashurup.quf.theme :as theme]
            [shashurup.quf.utils :as u]
            [shashurup.quf.vars :as vars]
            [xtermjs]))

(u/begin-module-load! :terminal)

(defn create-theme []
  (let [t (theme/get-theme)]
    {:background (:alt-bg t)
     :foreground (:fg t)
     :selectionBackground (:sel-bg t)
     :selectionInactiveBackground (:sel-bg t)}))

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
  (vars/push-server-updates! '*term-dimensions* (terminal-dimensions)))

(defn get-out-element [id]
  (gdom/getElement (str "out-" id)))

(defn send-stdin [subj]
  (nrepl/send-op {:op "stdin"
                  :stdin subj}
                 nil))

(defn send-command [cmd arg]
  (binding [*print-meta* true]
    (send-stdin (pr-str (with-meta arg {:cmd cmd})))))

(defn handle-key [subj]
  (send-stdin (pr-str (.-key subj))))

(defn handle-resize [_]
  (send-terminal-dimensions))

(defonce terminals (atom {}))

(defn shrink-terminal [terminal amount]
  (let [cols (.-cols terminal)
        rows (.-rows terminal)]
    (when (> rows amount)
      (.resize terminal cols (- rows amount)))))

(defn shrink-to-content
  "When the terminal content doesn't occupy it completely
   we just shrink it - we don't want this blank space really"
  [terminal]
  (let [el (gdom/getElementByClass "xterm-rows" (.-element terminal))]
    (->> el
         gdom/getChildren
         reverse
         (take-while #(nil? (.-firstChild %)))
         count
         (shrink-terminal terminal))))

;; TODO handle terminal deactivation
(defn deactivate-terminal [id]
  (let [terminal (get @terminals id)]
    (.onKey terminal nil)
    ;; At the moment terminal contents isn't rendred completely yet
    ;; So we postpone shrinking
    (js/setTimeout #(shrink-to-content terminal) 128)
    (swap! terminals dissoc id)))

(defn write-terimnal [id {:keys [out err status] :as reply}]
  (when-let [terminal (get @terminals id)]
    (if (nrepl/terminated? status)
      (deactivate-terminal id)
      (.write terminal (or out err)))))

(defn plug-terminal [id _]
  (. js/console debug "plugging terminal" id)
  (let [el (get-out-element id)
        [cols rows] (terminal-dimensions)
        terminal (js/Terminal. (clj->js {:convertEol true
                                         :fontFamily (first font)
                                         :fontSize (second font)
                                         :theme (create-theme)}))]
    (swap! terminals assoc id terminal)
    (swap! cell-handlers assoc id write-terimnal)
    (.resize terminal cols rows)
    (gevents/listen js/window
                    "resize"
                    (fn [_]
                      (let [[cols rows] (terminal-dimensions)]
                        (if (get @terminals id)
                          (do (.resize terminal cols rows)
                              (send-command :resize [cols rows]))
                          (.resize terminal cols (.-rows terminal))))))
    (.open terminal el)
    (.onKey terminal handle-key)
    (.focus terminal)))

(defonce startup-dummy 
  (do
    (swap! output-handlers assoc :terminal plug-terminal)
    (gevents/listen js/window "resize" handle-resize)
    ;; Postpone updating *terminal-dimensions* var
    ;; to avoid race condition modifying session local value
    (gevents/listenOnce js/document
                        "evalComplete"
                        send-terminal-dimensions)
    (u/add-style-ref "css/xterm.css")))

(u/set-module-loaded!)
