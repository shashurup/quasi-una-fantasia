(ns rackushka.history
  (:require [rackushka.config :as cfg]
            [clojure.edn :as edn]
            [clojure.string :as s]
            [nrepl.middleware :as mwre]
            [nrepl.misc :as m]
            [nrepl.transport :as t]))


(defonce history (agent (or (cfg/read "history") [])))

(defn includes-all-substrings? [subj substrings]
  (every? #(s/includes? subj %) substrings))

(defn history-matches [terms]
  (->> @history
       rseq
       (filter #(includes-all-substrings? % terms))
       distinct))

(defn- append [history code]
  (let [result (conj history code)]
    (cfg/write "history" result)
    result))

(defn log [op result]
  (when (and (= (:op op) "eval")
             (not-empty (filter #(contains? % :value) result)))
    (send-off history append (:code op)))) ;; todo save history

(defn wrap-history [h]
  (fn [{:keys [op transport terms limit] :as msg}]
    (if (= op "history")
      (t/send transport
              (m/response-for msg
                              :status :done
                              :matches (take (or limit 8)
                                             (history-matches terms))))
      (h msg))))

(mwre/set-descriptor! #'wrap-history
                      {:handles {"history" {:doc "Searches eval history"
                                            :requires {"terms" "Substrings to filter the history"}
                                            :optional {"limit" "Maximim number of matches to return, default is 8"}}}})
