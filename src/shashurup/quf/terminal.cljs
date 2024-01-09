(ns shashurup.quf.terminal
  (:require [goog.dom :as gdom]
            [goog.events :as gevents]
            [shashurup.quf.nrepl :as nrepl]
            [shashurup.quf.render :refer [handle-extra-data
                                          handle-output
                                          set-type!]]
            [shashurup.quf.utils :as u]
            [xterm]))

(defn get-out-element [id]
  (gdom/getElement (str "out-" id)))

(defn handle-key [subj]
  (.log js/console subj)
  (nrepl/send-op {:op "stdin"
                  :stdin (.-key subj)}
                 nil))

(defmethod handle-extra-data :terminal [_ id]
  (let [el (get-out-element id)
        terminal (xterm/Terminal. #js {:convertEol true})]
    (set-type! el :terminal)
    (set! (.-quf-terminal el) terminal)
    (.open terminal el)
    (.onKey terminal handle-key)
    (.focus terminal)))

(defmethod handle-output :terminal [target reply]
  (.write (.-quf-terminal target)
          (some reply [:out :err])))

(u/add-style-ref "css/xterm.css")
