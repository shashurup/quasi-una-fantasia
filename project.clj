(defproject quasi-una-fantasia "0.13-SNAPSHOT"
  :description "Quasi una fantasia is a graphical lisp shell prototype"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.11.1"]
                 [ring/ring-core "1.11.0"]
                 [ring/ring-jetty-adapter "1.9.1"]
                 ;; 0.4.0 by default uses cookie-store we don't need
                 [ring/ring-defaults "0.3.4"] 
                 [nrepl "1.1.0"]
                 [compojure "1.7.1"]
                 [cheshire "5.12.0"]
                 [org.clojure/data.csv "1.0.1"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [de.swiesend/secret-service "1.8.1-jdk17"]
                 [org.postgresql/postgresql "42.7.1"]
                 [com.novemberain/monger "3.6.0"]
                 [org.apache.tika/tika-core "2.9.1"]
                 [org.apache.tika/tika-parsers-standard-package "2.9.1"]
                 [crate "0.2.5"]
                 [cljsjs/openlayers "7.5.2-0"]
                 [cljsjs/chartjs "3.9.1-0"]
                 [org.jetbrains.pty4j/pty4j "0.12.25"]
                 [clj-commons/clj-yaml "1.0.27"]]

  :repositories [["JCenter" "https://jcenter.bintray.com/"]]
  
  :source-paths ["src"]

  :main shashurup.quf.srv

  :aot [shashurup.quf.srv]

  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:uber"  ["trampoline" "run" "-m" "figwheel.main"
                         "-O" "simple" "-fwo" "{:target-dir \"ubercljs\"}" "-bo" "dev"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "shashurup.quf.test-runner"]
            "base16"    ["run" "-m" "shashurup.quf.base16" "resources/base16.edn"]}

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.11.132"]
                                  [com.bhauman/figwheel-main "0.2.18"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]
                   
                   :resource-paths ["target"]
                   ;; need to add the compiled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["target" "ubercljs"]}
             :uberjar {:resource-paths ["ubercljs"]
                       :prep-tasks [["with-profile" "dev" "run" "-m" "figwheel.main"
                                     "-O" "simple" "-fwo" "{:target-dir \"ubercljs\"}" "-bo" "dev"]]}})

