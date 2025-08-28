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

(defn client-modules []
  (->> (loaded-libs)
       (map meta)
       (map :shashurup.quf/client-module)
       (filter identity)))

(defn- prunable? [subj]
   (and (sequential? subj)
        (coll? (first subj))))

(defn- map-map [subj f]
   (update-vals subj (fn [subj]
                       (cond (map? subj) (map-map subj f)
                             (prunable? subj) (f subj)
                             :else subj))))

(defn- prunable-count [subj]
   (->> (tree-seq map? vals subj)
        (filter prunable?)
        count))

(defn- consume-items [size subj]
  (let [items (take (inc size) subj)]
    [(take size items) (> (count items) size)]))

(defn prune-tree [subj size]
  (if (map? subj)
    (let [children-count (prunable-count subj)
          remainder (- size children-count)
          child-size (quot (if (neg? remainder) 0 remainder)
                           (if (zero? children-count) 1 children-count))]
      (map-map subj #(prune-tree % child-size)))
    (if (prunable? subj)
      (let [map-fn (if (vector? subj) mapv map)
            [children more?] (consume-items size subj)
            children-count (count children)
            range (if (pos? children-count)
                    {:from 0 :to (dec children-count)}
                    :emtpy)
            remainder (- size children-count)
            child-size (quot (if (neg? remainder) 0 remainder)
                             (if (zero? children-count) 1 children-count))]
        (vary-meta (map-fn #(prune-tree % child-size) children)
                   merge (when more? {:shashurup.quf/range range})))
      subj)))
