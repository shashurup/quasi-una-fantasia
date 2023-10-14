(ns shashurup.quf.selection
  (:require [nrepl.middleware :as mwre]
            [nrepl.misc :as m]
            [nrepl.transport :as t]))

(def ^:dynamic *selection* [{} nil])

(def ^:dynamic *s [])

(defn- add-items [subj additions]
  (reduce (fn [m [k v _]]
            (update m k #(conj (or % #{}) v)))
          subj
          additions))

(defn- remove-items [subj deletions]
  (reduce (fn [m [k v _]]
            (if v
              (update m k #(disj % v))
              (dissoc m k)))
          subj
          deletions))

(defn- update-selection [session updates]
  (let [[cell-map current] (get session (var *selection*))
        additions (filter #(nth % 2) updates)
        new-current (or (first (last additions)) current)
        new-cell-map (-> cell-map
                         (add-items additions)
                         (remove-items (remove #(nth % 2) updates)))]
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
