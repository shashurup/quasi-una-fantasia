(ns rackushka.geo
  (:require [clojure.set :as set]))

(defn normalize-data [data]
  (cond
    (string? data) [{:geometry data}]
    (and (coll? data)
         (string? (first data))) (mapv #(assoc {} :geometry (str %)) data)
    (coll? data) (mapv #(update (set/rename-keys % {:g :geometry
                                                    :geom :geometry
                                                    :name :label
                                                    :type :tag})
                                :geometry str) data)))

(defn v [subj]
  (with-meta
    (normalize-data subj)
    {:rackushka/hint :geodata}))
