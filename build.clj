(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.pprint :refer [pprint]]))

(def lib 'shashurup/quasi-una-fantasia)
(def version "0.13")

(def target "target")
(def slim-jar-dir (format "%s/slim-jar" target))
(def slim-jar-file (format "%s/%s-%s.jar" target (name lib) version))
(def uber-dir (format "%s/uberjar" target))
(def uber-file (format "%s/%s-%s-uber.jar" target (name lib) version))

(defn clean [_]
  (b/delete {:path target}))

(defn copy-dirs [sources target]
  (doseq [src sources]
    (b/copy-dir (assoc src :target-dir target))))

(defn slim-jar [_]
  (b/process {:command-args ["clj" "-M:cljs:slim-bo"]})
  (b/write-pom {:class-dir slim-jar-dir
                :lib lib
                :version version
                :basis (b/create-basis)})
  (copy-dirs [{:src-dirs ["src"] :include "**/*.clj"}
              {:src-dirs ["resources"] :ignores [#".*html"]}
              {:src-dirs [target] :include "public/cljs-out/slim-main.js"}]
             slim-jar-dir)
  (b/copy-file {:src "resources/public/slim.html"
                :target (format "%s/public/index.html" slim-jar-dir)})
  (b/jar {:class-dir slim-jar-dir
          :jar-file slim-jar-file}))

(defn install-slim-jar [_]
  (b/install {:lib lib
              :version version
              :class-dir slim-jar-dir
              :basis (b/create-basis)
              :jar-file slim-jar-file}))

(defn uber [_]
  (let [basis (b/create-basis {:aliases [:extra]})]
    (b/process {:command-args ["clj" "-M:extra:cljs:fat-bo"]})
    (b/write-pom {:class-dir uber-dir
                  :lib lib
                  :version version
                  :basis basis})
    (copy-dirs [{:src-dirs ["src" "extra/src"] :include "**/*.clj"}
                {:src-dirs ["resources"] :ignores [#".*html"]}
                {:src-dirs ["extra/resources"]}
                {:src-dirs [target] :include "public/cljs-out/fat-*.js"}
                {:src-dirs [target] :include "public/cljs-out/fat/cljs_base.js"}]
               uber-dir)
    (b/copy-file {:src "resources/public/fat.html"
                  :target (format "%s/public/index.html" uber-dir)})
    (b/compile-clj {:basis basis
                    :class-dir uber-dir
                    :ns-compile '[shashurup.quf.srv]})
    (b/uber {:class-dir uber-dir
             :uber-file uber-file
             :basis basis
             :main 'shashurup.quf.srv})))
