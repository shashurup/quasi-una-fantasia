(ns shashurup.quf.vars
  (:require [nrepl.middleware :as mwre]
            [nrepl.misc :as m]
            [nrepl.transport :as t]))

(def ^:dynamic *selection* [{} nil])

(def ^:dynamic *s [])

(def ^:dynamic *term-dimensions* [80 24])

(defn update-selection [[cell-map cur] [cell id add]]
  (if add
    [(update cell-map cell #(conj (or % #{}) id)) cell]
    [(if id
       (update cell-map cell disj id)
       (dissoc cell-map cell)) cur]))

(defn set-current-selection [_]
  (when-let [current (second *selection*)]
    (get (first *selection*) current)))

(defn- find-sym [subj]
  ((ns-map 'shashurup.quf.vars) subj))

(defn apply-updates [session updates]
  (reduce (fn [session [sym f & args]]
            (let [f (or (find-sym f) (constantly (first args)))
                  sym (find-sym sym)]
              (if (and sym f)
                (with-bindings session
                  (apply update session sym f args))
                session))) session updates))

(defn wrap-update-vars [h]
  (fn [{:keys [op session updates transport] :as msg}]
    (if (= op "update-vars")
      (do
        (swap! session apply-updates (read-string updates))
        (t/send transport (m/response-for msg :status :done)))
      (h msg))))

(mwre/set-descriptor! #'wrap-update-vars
                      {:require #{"clone"}
                       :handles {"update-vars" {:doc "Updates session vars bindings"
                                                :requires {"updates" "list of updates/new values"}}}})