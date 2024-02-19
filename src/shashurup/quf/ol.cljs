(ns shashurup.quf.ol
  (:require [cljs.loader :as loader]
            [shashurup.quf.render :refer [render]]
            [crate.core :as crate]
            [goog.dom :as gdom]
            [cljsjs.openlayers]))

(def colors ["#009" "#900" "#090" "#099" "#909" "#990"
             "#00f" "#f00" "#0f0" "#0ff" "#f0f" "#ff0" "#f80" "#f08"])

(def black-color "#000")

(defn read-wkb [subj proj]
  (.readFeature (js/ol.format.WKB.) subj #js {:featureProjection proj}))

(defn set-props [subj props]
  (.setProperties subj (clj->js props))
  subj)

(defn make-feature [subj proj]
  (let [wkb (:geometry subj)]
    (when (not-empty wkb)
      (set-props (read-wkb wkb proj)
                 (dissoc subj :geometry)))))

(defn make-style [feature color-map]
  (js/ol.style.Style.
   #js {:fill (js/ol.style.Fill. #js {:color "rgba(255, 255, 255, 0.4)"})
        :stroke (js/ol.style.Stroke. #js {:color (get color-map (.get feature "tag"))})
        :text (js/ol.style.Text. #js {:text (.get feature "label")})}))

(defn make-source [subj proj]
  (js/ol.source.Vector.
   (clj->js {:features (->> subj
                            (map #(make-feature % proj))
                            (remove nil?)
                            (into []))})))

(defn create-map-control [target geodata]
  (let [color-map (zipmap (distinct (map :tag geodata))
                          (concat colors (repeat black-color)))
        view (js/ol.View. (clj->js {:padding [32 32 32 32]}))
        osm-tiles (js/ol.layer.Tile. (clj->js {:source (js/ol.source.OSM.)}))
        src (make-source geodata (.getProjection view))
        subj-layer (js/ol.layer.Vector. (clj->js {:source src
                                                  :style #(make-style % color-map)}))]
    (js/ol.Map. (clj->js {:layers [osm-tiles subj-layer]
                          :target target
                          :view view}))
    (.fit view (.getExtent src))))

(defmethod render :geodata [subj]
  (fn [target]
    (let [c (crate/html [:div.quf-medium-sized])]
      (gdom/appendChild target c)
      (create-map-control c subj))))

(loader/set-loaded! :ol)
