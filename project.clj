(defproject quasi-una-fantasia "0.12-SNAPSHOT"
  :description "Quasi una fantasia is a graphical lisp shell prototype"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/clojurescript "1.11.60"]
                 [compojure "1.6.2"]
                 [nrepl/drawbridge "0.2.1"]
                 [cheshire "5.11.0"]
                 [org.clojure/data.csv "1.0.1"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [de.swiesend/secret-service "1.8.1-jdk17"]
                 [org.postgresql/postgresql "42.5.1"]
                 [com.novemberain/monger "3.6.0"]
                 [org.apache.tika/tika-core "2.7.0"]
                 [org.apache.tika/tika-parsers-standard-package "2.7.0"]
                 [crate "0.2.4"]
                 [cljsjs/openlayers "7.2.2-0"]
                 [cljsjs/chartjs "3.9.1-0"]
                 [org.jetbrains.pty4j/pty4j "0.12.25"]
                 ]

  :repositories [["JCenter" "https://jcenter.bintray.com/"]]
  
  :source-paths ["src"]

  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "shashurup.quf.test-runner"]}

  :profiles {:dev {:dependencies [[com.bhauman/figwheel-main "0.2.18"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]
                   
                   :resource-paths ["target"]
                   ;; need to add the compiled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["target"]}})

