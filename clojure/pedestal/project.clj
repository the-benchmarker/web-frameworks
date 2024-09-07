(defproject the-benchmarker-pedestal "0.0.1-SNAPSHOT"
  :description ""
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.4"]
                 [io.pedestal/pedestal.service "0.7.0"]
                 [io.pedestal/pedestal.jetty "0.7.0"]]
  :min-lein-version "2.0.0"
  :resource-paths []
  :profiles {:uberjar {:aot [the-benchmarker-pedestal.server]}}
  :main ^{:skip-aot true} the-benchmarker-pedestal.server)
