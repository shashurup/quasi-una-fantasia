(ns shashurup.quf.evt
  (:require [shashurup.quf.response :as r]
            [nrepl.middleware :as mwre]
            [nrepl.middleware.session :refer [session]]
            [nrepl.transport :as t]))

(defn wrap-event-reporter [h]
  (fn [{:keys [id session op transport] :as msg}]
    (when (= op "eval")
      (let [session-id (:id (meta session))
            re (fn [subj]
                 (t/send transport
                         {:id id
                          :session session-id
                          :event (binding [*print-meta* true]
                                   (pr-str subj))})
                 nil)
            ev-start (fn []
                       (t/send transport
                               {:id id
                                :session session-id
                                :status #{:expect-background-events}}))
            ev-stop (fn []
                      (t/send transport
                              {:id id
                               :session session-id
                               :status #{:no-more-background-events}}))]
        (swap! session assoc
               #'r/report-event re
               #'r/expect-background-events ev-start
               #'r/no-more-background-events ev-stop)))
    (h msg)))

(mwre/set-descriptor! #'wrap-event-reporter
                      {:expects #{"eval"}
                       :requires #{#'session}
                       :handles {"set-event-reporting-callback"
                                 {:doc "Sets up report-event callback"}}})
