(ns shashurup.quf.desc
  (:require [clojure.string :as s]))

(defonce field-types
  (atom {}))

(def unit-map [["T" (* 1024 1024 1024 1024)]
               ["G" (* 1024 1024 1024)]
               ["M" (* 1024 1024)]
               ["K" 1024]
               [""  1]])

(defn- grouped-file-size [subj]
  (.format (js/Intl.NumberFormat.) subj))

(defn- human-readable-file-size [subj]
  (let [[size unit]
        (->> unit-map
             (map (fn [[unit size]]
                    [(/ subj size) unit]))
             (filter (fn [[size _]] (>= size 1)))
             first)]
    (str (if (< size 10)
           (/ (js/Math.round (* size 10)) 10)
           (js/Math.round size))
         unit)))

(def file-size {:convert human-readable-file-size
                :alt [{:convert grouped-file-size}]})

(swap! field-types assoc ::file-size file-size)

(def datetime-formatter
  (js/Intl.DateTimeFormat. "en" #js {"hour12" false
                                     "year" "numeric"
                                     "month" "short"
                                     "day" "numeric"
                                     "hour" "2-digit"
                                     "minute" "2-digit"}))

(defn- datetime-parts [subj]
  (->> (.formatToParts datetime-formatter subj)
       (map #(vector (keyword (.-type %)) (.-value %)))
       (into {})))

(defn- compact-time-from-milliseconds [subj]
  (let [date (js/Date. subj)
        this-year (.getFullYear (js/Date.))
        year-ago (.setFullYear (js/Date.) (- this-year 1))
        parts (datetime-parts date)
        {M :month d :day y :year h :hour m :minute} parts]
    (if (< subj year-ago)
      (str M " " (.padStart d 2) " " y)
      (str M " " (.padStart d 2) " " h ":" m))))

(defn- full-time-from-milliseconds [subj]
  (.toISOString (js/Date. subj)))

(def milliseconds {:convert compact-time-from-milliseconds
                   :alt [{:convert full-time-from-milliseconds}]})

(swap! field-types assoc ::millisecons milliseconds)

(defonce object-types
  (atom {}))

(defn- kw2s [subj]
  (if (number? subj)
    (str subj)
    (s/capitalize (subs (str subj) 1))))

(defn- column-defaults [key]
  {:key key
   :title (kw2s key)
   :convert identity})

(defn- htmlize [subj]
  (when subj
    (fn [arg]
      (with-meta (subj arg) {:shashurup.quf/hint :html}))))

(defn canonize-column [key desc]
  (let [m (cond
              (keyword? desc) {:type desc}
              (fn? desc)      {:convert desc}
              :else           desc)]
    (merge (column-defaults key)
           (get @field-types (:type m))
           m)))

(defn make-getter [key]
  (cond
    (keyword? key) #(get % key)
    (number?  key) #(nth % key)
    (vector?  key) (fn [row]
                     (vec (for [k key]
                            ((make-getter k) row))))
    :else (fn [_] :broken-key)))

(defn make-renderer [column-desc]
  (comp (or (htmlize (:render column-desc)) identity)
        (:convert column-desc)
        (make-getter (:key column-desc))))

(defn- pack-table-desc [columns]
  [(mapv :title columns) (mapv make-renderer columns)])

(defn table-desc
  ([type-key columns]
   (let [desc (get-in @object-types [type-key :columns])]
     (pack-table-desc (for [col columns]
                        (canonize-column col (get desc col))))))
  ([columns]
   (pack-table-desc (for [col columns]
                      (if (map? col)
                        (canonize-column {:key col} col)
                        (canonize-column col nil))))))
