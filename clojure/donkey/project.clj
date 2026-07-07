(defproject the-benchmarker-donkey "0.1.0-SNAPSHOT"
  :description "Production-grade Donkey web server"
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [org.clojure/tools.logging "1.3.1"]
                 [com.appsflyer/donkey "0.5.2"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"
                       "-XX:+UseStringDeduplication"
                       "-XX:+UseParallelGC"
                       "-XX:GCTimeRatio=4"
                       "-XX:AdaptiveSizePolicyWeight=90"
                       "-XX:MaxGCPauseMillis=200"]
  :uberjar-name "donkey.jar"
  :target-path "target/%s/"
  :aot :all
  :main d.server
  :profiles {:uberjar {:aot :all
                       :omit-source true}})
