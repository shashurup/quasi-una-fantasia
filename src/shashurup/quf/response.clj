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

(defn report-progress [message value max]
  (print-with-hint [message value max] :progress))

(defn client-require [ns]
  (print-with-hint [ns] :require))
