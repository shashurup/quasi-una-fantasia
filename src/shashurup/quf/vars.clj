(ns shashurup.quf.vars
  (:require [nrepl.middleware :as mwre]
            [nrepl.misc :as m]
            [nrepl.transport :as t]))

(def ^:dynamic *term-dimensions* [80 24])

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
  (fn [{:keys [op session var-updates transport] :as msg}]
    (when (and (= op "eval") var-updates)
        (swap! session apply-updates (read-string var-updates)))
    (h msg)))

(mwre/set-descriptor! #'wrap-update-vars
                      {:requires #{"session"}
                       :expects #{"eval"}
                       :handles {"update-vars" {:doc "Updates session vars bindings"
                                                :requires {"updates" "list of updates/new values"}}}})
