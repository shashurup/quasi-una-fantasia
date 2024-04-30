(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.pprint :refer [pprint]]))

(def lib 'shashurup/quasi-una-fantasia)
(def version "0.13")

(def target "target")
(def slim-jar-dir (format "%s/slim-jar" target))
(def slim-jar-file (format "%s/%s-%s.jar" target (name lib) version))

(defn clean [_]
  (b/delete {:path target}))

(defn slim-jar [_]
  (b/process {:command-args ["clj" "-M:fig:slim-bo"]})
  (b/write-pom {:class-dir slim-jar-dir
                :lib lib
                :version version
                :basis (b/create-basis)})
  (b/copy-dir {:src-dirs ["src"]
               :include "**/*.clj"
               :target-dir slim-jar-dir})
  (b/copy-dir {:src-dirs ["resources"]
               :ignores [#".*html"]
               :target-dir slim-jar-dir})
  (b/copy-file {:src "resources/public/slim.html"
                :target (format "%s/public/index.html" slim-jar-dir)})
  (b/copy-file {:src "target/public/cljs-out/slim-main.js"
                :target (format "%s/public/cljs-out/slim-main.js" slim-jar-dir)})
  (b/jar {:class-dir slim-jar-dir
          :jar-file slim-jar-file}))

(defn install-slim-jar [_]
  (b/install {:lib lib
              :version version
              :class-dir slim-jar-dir
              :basis (b/create-basis)
              :jar-file slim-jar-file}))
