(defproject the-benchmarker-pedestal "0.0.1-SNAPSHOT"
  :description ""
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [io.pedestal/pedestal.service "0.6.1"]
                 [io.pedestal/pedestal.jetty "0.6.1"]]
  :min-lein-version "2.0.0"
  :resource-paths []
  :profiles {:uberjar {:aot [the-benchmarker-pedestal.server]}}
  :main ^{:skip-aot true} the-benchmarker-pedestal.server)
