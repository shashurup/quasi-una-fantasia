(ns shashurup.quf.debug)

(defn show [node]
  (when node
    (cond
      (coll? node) (map show node)
      (= (.-nodeType node) 1) (.-outerHTML node)
      (= (.-nodeType node) 3) (.-textContent node)
      :else node)))
