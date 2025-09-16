(ns shashurup.quf.pruner
  (:require [nrepl.middleware :as mwre]
            [nrepl.middleware.print :refer [wrap-print]]
            [nrepl.transport :as t])
  (:import (nrepl.transport Transport)))

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

(defn- prune-tree
  ([subj size] (prune-tree subj size 0))
  ([subj size from]
   (let [original-meta (meta subj)]
     (cond
       (:shashurup.quf/range original-meta) subj

       (map? subj)
       (let [children-count (prunable-count subj)
             remainder (- size children-count)
             child-size (quot (if (neg? remainder) 0 remainder)
                              (if (zero? children-count) 1 children-count))]
         (with-meta (map-map subj #(prune-tree % child-size))
           original-meta))

       (prunable? subj)
       (let [map-fn (if (vector? subj) mapv map)
             [children more?] (consume-items size subj)
             children-count (count children)
             range {:from from
                    :to (+ from children-count)
                    :more? more?}
             remainder (- size children-count)
             child-size (quot (if (neg? remainder) 0 remainder)
                              (if (zero? children-count) 1 children-count))]
         (with-meta (map-fn #(prune-tree % child-size) children)
           (merge original-meta {:shashurup.quf/range range})))

       :else subj))))

(defn- extract [subj path]
  (let [[k & rest] path]
    (if (nil? k)
      subj
      (cond
        (map? subj) (extract (get subj k) rest)
        (coll? subj) (extract (nth subj k) rest)))))

(defn- get-range [subj path from to]
  (when-let [data (extract subj path)]
    (let [rem (drop (or from 0) data)
          original-meta (meta data)]
      (if to
        (prune-tree (with-meta rem original-meta) (- to from) from)
        (with-meta
          (map #(prune-tree % 0) rem) original-meta)))))

(defn- pruning-transport [transport {quota :shashurup.quf/quota
                                     path :shashurup.quf/path
                                     {from :from to :to} :shashurup.quf/range}]
  (reify Transport
    (recv [_] (t/recv transport))
    (recv [_ timeout] (t/recv transport timeout))
    (send [_ {:keys [value] :as resp}]
      (if (contains? resp :value)
        (cond
          quota (t/send transport
                        (update resp :value prune-tree quota))
          path (t/send transport
                       (update resp :value get-range path from to))
          :else (t/send transport resp))
        (t/send transport resp)))))

(defn wrap-pruner [h]
  (fn [{:keys [transport] :as msg}]
    (let [opts (select-keys msg [:shashurup.quf/quota
                                 :shashurup.quf/path
                                 :shashurup.quf/range])]
      (if opts
        (h (assoc msg :transport (pruning-transport transport opts)))
        (h msg)))))

(mwre/set-descriptor! #'wrap-pruner
                      {:expects #{"eval"}
                       :requires #{#'wrap-print}
                       :handles {}})
