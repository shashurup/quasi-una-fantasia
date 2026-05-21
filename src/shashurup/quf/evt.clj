(ns shashurup.quf.evt
  (:require [shashurup.quf.response :as r]
            [nrepl.middleware :as mwre]
            [nrepl.middleware.session :refer [session]]
            [nrepl.transport :as t]))

(defonce cancel-fns (atom {}))

(defn wrap-event-reporter [h]
  (fn [{:keys [id session op] :as msg}]
    (let [session-id (:id (meta session))]
      (condp = op
        "eval" (let [re (fn [subj]
                          (t/respond-to msg :event (binding [*print-meta* true]
                                                     (pr-str subj)))
                          nil)
                     ev-start (fn [cancel]
                                (swap! cancel-fns
                                       assoc-in [session-id id] cancel)
                                (t/respond-to msg
                                              :status
                                              #{:expect-background-events})
                                nil)
                     ev-stop (fn []
                               (t/respond-to msg
                                             :status
                                             #{:no-more-background-events})
                               nil)]
                 (swap! session assoc
                        #'r/report-event re
                        #'r/expect-background-events ev-start
                        #'r/no-more-background-events ev-stop)
                 (h msg))
        "cancel-events" (if-let [cancel (get-in @cancel-fns [session-id id])]
                          (do (swap! cancel-fns update session-id dissoc id)
                              (cancel)
                              (t/respond-to msg :status #{:done}))
                          (t/respond-to msg :status #{:done :error :not-found}))
        (h msg)))))

(mwre/set-descriptor! #'wrap-event-reporter
                      {:expects #{"eval"}
                       :requires #{#'session}
                       :handles {"cancel-events"
                                 {:doc "Cancel events sent by a producer"}}})
