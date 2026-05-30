(ns shashurup.quf.view
  (:require [clojure.string :as s]))

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

(defn table
  ([cols subj] (hint subj [:table cols]))
  ([subj] (hint subj :table)))

(defn tree
  ([opts subj] (hint subj [:tree opts]))
  ([subj] (hint subj :tree)))

(defn pack [subj] (hint subj :sequence))

(defn text [subj]
  (if (string? subj)
    (hint [subj] :text)
    (hint subj :text)))

(defn code [subj & args]
  (let [hint_ (if (empty? args)
               :code
               [:code args])]
    (if (string? subj)
      (hint [subj] hint_)
      (hint subj hint_))))

(defn html [subj] (hint subj :html))

(defn keymap [subj] (hint subj :keymap))

(defn raw [subj] (vary-meta subj dissoc :shashurup.quf/hint))

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

(defn client-modules []
  (->> (loaded-libs)
       (map meta)
       (map :shashurup.quf/client-module)
       (filter identity)))

(defn load-cells [source]
  (hint (if (symbol? source)
          source
          [(slurp source)]) :cells))

(defn store-cells [path cells]
  (spit path (s/join "\n\n" cells)))
