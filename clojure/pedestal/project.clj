(defproject the-benchmarker-pedestal "0.0.1-SNAPSHOT"
  :description "Production-grade Pedestal web server"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [org.clojure/tools.logging "1.3.1"]
                 [io.pedestal/pedestal.service "0.8.1"]
                 [io.pedestal/pedestal.jetty "0.8.1"]]
  :min-lein-version "2.0.0"
  :resource-paths []
  :jvm-opts ["-XX:+UseG1GC" "-XX:MaxGCPauseMillis=200" "-XX:+DisableExplicitGC"]
  :profiles {:uberjar {:aot [the-benchmarker-pedestal.server]
                       :omit-source true}}
  :main ^{:skip-aot true} the-benchmarker-pedestal.server)
