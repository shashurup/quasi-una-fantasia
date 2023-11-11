(ns shashurup.quf.debug)

(defn show [node]
  (cond
    (= (.-nodeType node) 1) (.-outerHTML node)
    (= (.-nodeType node) 3) (.-textContent node)
    :else node))
