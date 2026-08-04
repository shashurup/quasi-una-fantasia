(ns shashurup.quf.sweeper
  (:require [shashurup.quf.view :as v]
            [nrepl.middleware :as mwre]
            [nrepl.middleware.print :refer [wrap-print]]
            [nrepl.middleware.session :refer [session]]
            [nrepl.transport :as t])
  (:import (nrepl.transport Transport)))

(defn- sweeping-transport [transport sweep-list]
  (reify Transport
    (recv [_] (t/recv transport))
    (recv [_ timeout] (t/recv transport timeout))
    (send [_ {:keys [status] :as resp}]
      (when (:done status)
        (doseq [f @sweep-list]
          (try
            (f)
            (catch Exception e
              (println e)))))
      (t/send transport resp))))

(defn wrap-sweeper [h]
  (fn [{:keys [session op transport] :as msg}]
    (if (= op "eval")
        (let [sweep-list (atom nil)]
          (swap! session assoc
                 #'v/defer #(swap! sweep-list conj %))
          (h (assoc msg :transport (sweeping-transport transport sweep-list))))
        (h msg))))

(mwre/set-descriptor! #'wrap-sweeper
                      {:expects #{#'wrap-print}
                       :requires #{#'session}
                       :handles {}})
