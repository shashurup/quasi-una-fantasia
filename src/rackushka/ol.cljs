(ns rackushka.ol
  (:require [rackushka.core :as c] ; todo mover render to a separate ns
            [goog.dom :as gdom]
            [cljsjs.openlayers]))

(defn make-source [subj]
  (js/ol.source.Vector.
   (clj->js {:features
             (mapv #(.readFeature (js/ol.format.WKB.)
                                  (str (:geometry %))) subj)})))

(defn create-map-control [target src]
  (let [view (js/ol.View. (clj->js {:center [0 0] :zoom 1}))
        osm-tiles (js/ol.layer.Tile. (clj->js {:source (js/ol.source.OSM.)}))
        subj-layer (js/ol.layer.Vector. (clj->js {:source src}))]
    (js/ol.Map. (clj->js {:layers [osm-tiles subj-layer]
                          :target target
                          :view view}))
    (.fit view (.getExtent src) (clj->js {:padding [32 32 32 32]}))))

(defmethod c/render :geodata [subj]
  (fn [target]
    (let [c (gdom/createDom "div" #js {:style "width: 100%; height: 40vh"})]
      (gdom/appendChild target c)
      (create-map-control c (make-source subj)))))
