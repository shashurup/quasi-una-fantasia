(ns shashurup.quf.geo
  (:require [clojure.set :as set]
            [shashurup.quf.response :as resp]))

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
  (resp/hint (normalize-data subj) :geodata))

(resp/client-module :ol)
