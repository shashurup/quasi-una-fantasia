(ns rackushka.sh
  (:require [clojure.java.shell :as shell]
            [clojure.string :as s]
            [rackushka.fs :as fs]))

(defn ! [& args]
  (let [args (if (> (count args) 1)
               args
               (remove empty? (s/split (first args) #"\s")))]
    (let [{:keys [exit out err]} (apply shell/sh
                                        (concat args
                                                [:dir fs/*cwd*]))]
      (when (not-empty err)
        (print err))
      (if (= exit 0)
        (with-meta 
          (s/split-lines out)
          {:rackushka/hint :text})
        (throw (Exception. (str "Exit code " exit)))))))
