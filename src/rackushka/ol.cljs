(ns rackushka.ol
  (:require [rackushka.core :as c] ; todo mover render to a separate ns
            [goog.dom :as gdom]
            [cljsjs.openlayers]))

(defn make-source [subj proj]
  (js/ol.source.Vector.
   (clj->js {:features
             (mapv #(.readFeature (js/ol.format.WKB.)
                                  (:geometry %)
                                  #js {:featureProjection proj}) subj)})))

(defn create-map-control [target geodata]
  (let [view (js/ol.View. (clj->js {:padding [32 32 32 32]}))
        osm-tiles (js/ol.layer.Tile. (clj->js {:source (js/ol.source.OSM.)}))
        src (make-source geodata (.getProjection view))
        subj-layer (js/ol.layer.Vector. (clj->js {:source src}))]
    (js/ol.Map. (clj->js {:layers [osm-tiles subj-layer]
                          :target target
                          :view view}))
    (.fit view (.getExtent src))))

(defmethod c/render :geodata [subj]
  (fn [target]
    (let [c (gdom/createDom "div" #js {:style "width: 100%; height: 40vh"})]
      (gdom/appendChild target c)
      (create-map-control c subj))))
