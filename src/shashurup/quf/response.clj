(ns shashurup.quf.response
  (:require [nrepl.middleware :as mwre]
            [nrepl.misc :as m]
            [nrepl.transport :as t]))

(defn hint [obj hint]
  (when obj
    (vary-meta obj assoc :shashurup.quf/hint hint)))

(defn pr-with-meta [subj target _]
  (binding [*out* target
            *print-meta* true]
    (pr subj)))

(defn pr-str-meta [subj]
  (binding [*print-meta* true] (pr-str subj)))

(def ^:dynamic send-extra-data)

(defn report-progress [percent message]
  (if (bound? #'send-extra-data)
    (send-extra-data (hint {:percent percent
                            :message message} :progress))
    (println (str message ", " percent "%"))))

(defn client-require [ns]
  (when (bound? #'send-extra-data)
    (send-extra-data (hint {:ns ns} :require))))

(defn wrap-extra-data [h]
  (fn [{:keys [op session transport] :as msg}]
    (when (= op "eval")
      (let [send (fn [data]
                   (t/send transport
                           (m/response-for msg :x-data (pr-str-meta data))))]
        (swap! session assoc (var send-extra-data) send)))
    (h msg)))

(mwre/set-descriptor! #'wrap-extra-data
                      {:require #{"clone"}
                       :expects #{"eval"}
                       :handles {}})
