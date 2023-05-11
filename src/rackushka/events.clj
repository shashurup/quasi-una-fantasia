(ns rackushka.events
  (:require [nrepl.middleware :as mwre]
            [nrepl.misc :as m]
            [nrepl.transport :as t]))

(defonce event-queue (atom []))

(defn push [subj] (swap! event-queue conj subj))

(defn wrap-events [h]
  (fn [{:keys [op transport from] :as msg}]
    (condp = op
      "events" (t/send transport
                       (m/response-for msg
                                       :status :done
                                       :events (drop (or from 0)
                                                     @event-queue)))
      "eval" (do (t/send transport
                         (m/response-for msg
                                         :event-queue-size (count @event-queue)))
                 (h msg))
      (h msg))))

(mwre/set-descriptor! #'wrap-events
                      {:handles {"events" {:doc "Returns events pushed by the server"
                                           :requires {"from" "position in a queue from which to return events"}}}})
