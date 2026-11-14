(defproject the-benchmarker-http-kit "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.12.6"]
                 [http-kit "2.8.1"]
                 [metosin/reitit-ring "0.11.0"]]
  :min-lein-version "2.0.0"
  :uberjar-name "http-kit.jar"
  :target-path "target/%s/"
  :aot :all
  :main the-benchmarker-http-kit.server)
