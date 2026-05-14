(ns shashurup.quf.response)

(defn hint [obj hint]
  (when obj
    (vary-meta obj assoc :shashurup.quf/hint hint)))

(defn pr-with-meta [subj target _]
  (binding [*out* target
            *print-meta* true]
    (pr subj)))

(defn print-with-hint [subj hint_]
  (binding [*print-meta* true]
    (pr (hint subj hint_))
    (flush)))

;; Fallback event reporting function
;; Overriden in evt middleware
(def ^:dynamic report-event pr)

(defn report-progress
  ([message] (report-progress message nil nil))
  ([message value max]
   (report-event (merge {:type :progress
                         :message message}
                        (when value
                          {:value value
                           :max max})))))

(defn client-modules []
  (->> (loaded-libs)
       (map meta)
       (map :shashurup.quf/client-module)
       (filter identity)))
