(ns rackushka.geo
  (:require [clojure.set :as set]
            [rackushka.events :as events]))

(defn normalize-data [data]
  (cond
    (string? data) [{:geometry data}]
    (and (coll? data)
         (string? (first data))) (mapv #(assoc {} :geometry (str %)) data)
    (and (coll? data)
         (vector? (first data))) (mapv #(update (zipmap [:geometry :label :tag] %)
                                                :geometry
                                                str)
                                       data)
    (coll? data) (mapv #(update (set/rename-keys % {:g :geometry
                                                    :geom :geometry
                                                    :name :label
                                                    :type :tag})
                                :geometry str) data)))

(defn v
  "Show geometries on a map.
   subj could be:
        a string - geometry wkt
        a collection maps where
          :geometry, :g or :geom - wkt
          :label or :name to put on each object
          :tag or :type to automatically color objects
        a collection of vectors with [geometry label tag]"
  [subj]
  (with-meta
    (normalize-data subj)
    {:rackushka/hint :geodata}))

(events/push {:type :require :ns "rackushka.ol"})
