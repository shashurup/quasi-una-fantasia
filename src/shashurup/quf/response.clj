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

(def ^:dynamic expect-background-events (fn [cancel]))

(def ^:dynamic no-more-background-events (fn []))

;; Fallback event reporting function
;; Overriden in evt middleware
(def ^:dynamic report-event pr)

(def ^:dynamic session-id nil)

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

(defn track
  ([subj] (track subj identity))
  ([subj conv]
   (let [key (str "event-handler-" (random-uuid))
         re report-event
         nmbe no-more-background-events
         handler (fn [key subj _ new-state]
                   (try
                     (re {:type :value
                          :value (conv new-state)})
                     (catch Exception ex
                       (when (= (:reason (ex-data ex)) :timeout)
                         (remove-watch subj key)))))
         untrack (fn []
                   (nmbe)
                   (remove-watch subj key))]
     (expect-background-events untrack)
     (add-watch subj key handler)
     (conv @subj))))
