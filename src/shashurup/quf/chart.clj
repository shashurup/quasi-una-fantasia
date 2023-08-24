(ns shashurup.quf.chart)

(defn normalize [data]
  (for [x data]
    (if (coll? x) x [(str x) x])))

(defn show [data type] (with-meta (normalize data) {:shashurup.quf/hint [:chart type]}))

(defn bar [data] (show data :bar))

(defn line [data] (show data :line))

(defn pie [data] (show data :pie))

(defn doughnut [data] (show data :doughnut))

(defn scatter [data] (with-meta data {:shashurup.quf/hint [:chart :scatter]}))

(defn histogram
  ([data] (histogram data 11))
  ([n data]
   (let [from (apply min data)
         step (/ (- (apply max data) from) n)]
     (->> (reduce (fn [a v]
                    (let [k (quot (- v from) step)]
                      (update a k #(inc (or % 0))))) {} data)
          (map (fn [[k v]] [(+ from (* step k)) v]))
          (sort-by first)
          bar))))
