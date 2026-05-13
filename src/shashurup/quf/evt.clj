(ns shashurup.quf.evt
  (:require [shashurup.quf.response :as r]
            [nrepl.middleware :as mwre]
            [nrepl.middleware.session :refer [session]]
            [nrepl.transport :as t]))

(defn wrap-event-reporter [h]
  (fn [{:keys [id session op transport] :as msg}]
    (when (= op "eval")
      (let [re (fn [subj]
                 (t/send transport
                         {:id id
                          :session (:id (meta session))
                          :event (binding [*print-meta* true]
                                   (pr-str subj))})
                 nil)]
        (swap! session assoc #'r/report-event re)))
    (h msg)))

(mwre/set-descriptor! #'wrap-event-reporter
                      {:expects #{"eval"}
                       :requires #{#'session}
                       :handles {"set-event-reporting-callback"
                                 {:doc "Sets up report-event callback"}}})
