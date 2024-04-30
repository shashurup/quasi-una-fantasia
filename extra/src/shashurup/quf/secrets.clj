(ns shashurup.quf.secrets
  (:import [org.freedesktop.secret.simple SimpleCollection]))

(defn lookup [attrs]
  (with-open [c (SimpleCollection.)]
    (when-let [item (first (.getItems c attrs))]
      (String. (.getSecret c item)))))
