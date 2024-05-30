(defproject the-benchmarker-donkey "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [com.appsflyer/donkey "0.5.2"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :uberjar-name "donkey.jar"
  :target-path "target/%s/"
  :aot :all
  :main d.server)
