(ns shashurup.quf.selection
  (:require [nrepl.middleware :as mwre]
            [nrepl.misc :as m]
            [nrepl.transport :as t]))

(def ^:dynamic *selection* [{} nil])

(def ^:dynamic *s [])

(defn- apply-an-update [cell-map [cell id add]]
  (if add
    (update cell-map cell #(conj (or % #{}) id))
    (if id
      (update cell-map cell disj id)
      (dissoc cell-map cell))))

(defn- update-selection [session updates]
  (let [[cell-map current] (get session (var *selection*))
        new-current (or (->> updates
                             (filter (fn [[_ _ add]] add))
                             last
                             first)
                        current)
        new-cell-map (reduce apply-an-update cell-map updates)]
    (assoc session (var *selection*) [new-cell-map new-current]
                   (var *s) (seq (get new-cell-map
                                      new-current)))))

(defn wrap-selection [h]
  (fn [{:keys [op session selection-updates] :as msg}]
    (when (and (= op "eval") (not-empty selection-updates))
      (swap! session update-selection selection-updates))
    (h msg)))

(mwre/set-descriptor! #'wrap-selection
                      {:require #{"clone"}
                       :expects #{"eval"}
                       :handles {}})
