(ns rackushka.ol
  (:require [rackushka.core :as c] ; todo mover render to a separate ns
            [goog.dom :as gdom]
            [cljsjs.openlayers]))

(defn read-wkb [subj proj]
  (.readFeature (js/ol.format.WKB.) subj #js {:featureProjection proj}))

(defn set-props [subj props]
  (.setProperties subj (clj->js props))
  subj)

(defn make-style [feature]
  (js/ol.style.Style.
   #js {:fill (js/ol.style.Fill. #js {:color "rgba(255,255,255,0.6)"})
        :stroke (js/ol.style.Stroke. #js {:color "#00f"})
        :text (js/ol.style.Text. #js {:text (.get feature "label")})}))

(defn make-source [subj proj]
  (js/ol.source.Vector.
   (clj->js {:features
             (mapv #(set-props (read-wkb (:geometry %) proj)
                               (dissoc % :geometry)) subj)})))

(defn create-map-control [target geodata]
  (let [view (js/ol.View. (clj->js {:padding [32 32 32 32]}))
        osm-tiles (js/ol.layer.Tile. (clj->js {:source (js/ol.source.OSM.)}))
        src (make-source geodata (.getProjection view))
        subj-layer (js/ol.layer.Vector. (clj->js {:source src :style make-style}))]
    (js/ol.Map. (clj->js {:layers [osm-tiles subj-layer]
                          :target target
                          :view view}))
    (.fit view (.getExtent src))))

(defmethod c/render :geodata [subj]
  (fn [target]
    (let [c (gdom/createDom "div" #js {:style "width: 100%; height: 40vh"})]
      (gdom/appendChild target c)
      (create-map-control c subj))))
