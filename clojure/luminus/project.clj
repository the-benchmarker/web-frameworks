(defproject luminus "0.1.0-SNAPSHOT"

  :dependencies [[luminus/lein-template "4.51"]
                 [ch.qos.logback/logback-classic "1.5.6"]
                 [cheshire "5.13.0"]
                 [clojure.java-time "1.4.2"]
                 [cprop "0.1.20"]
                 [expound "0.9.0"]
                 [funcool/struct "1.4.0"]
                 [luminus-jetty "0.2.3"]
                 [luminus-transit "0.1.6"]
                 [luminus/ring-ttl-session "0.3.3"]
                 [markdown-clj "1.12.1"]
                 [metosin/muuntaja "0.6.10"]
                 [metosin/reitit "0.7.0"]
                 [metosin/ring-http-response "0.9.3"]
                 [mount "0.1.18"]
                 [nrepl "1.1.1"]
                 [org.clojure/clojure "1.11.3"]
                 [org.clojure/tools.cli "1.1.230"]
                 [org.clojure/tools.logging "1.3.0"]
                 [org.webjars.npm/bulma "0.9.4"]
                 [org.webjars.npm/material-icons "1.13.2"]
                 [org.webjars/webjars-locator "0.52"]
                 [ring-webjars "0.2.0"]
                 [ring/ring-core "1.12.1"]
                 [ring/ring-defaults "0.5.0"]
                 [com.fasterxml.jackson.core/jackson-core "2.17.1"]]

  :min-lein-version "2.0.0"
  
  :source-paths ["src/clj"]
  :test-paths ["test/clj"]
  :resource-paths ["resources"]
  :target-path "target/%s/"
  :main ^:skip-aot luminus.core

  :plugins [] 

  :profiles
  {:uberjar {:omit-source true
             :aot :all
             :uberjar-name "luminus.jar"
             :source-paths ["env/prod/clj" ]
             :resource-paths ["env/prod/resources"]}

   :dev           [:project/dev :profiles/dev]
   :test          [:project/dev :project/test :profiles/test]

   :project/dev  {:jvm-opts ["-Dconf=dev-config.edn" ]
                  :dependencies [[pjstadig/humane-test-output "0.11.0"]
                                 [prone "2021-04-23"]
                                 [ring/ring-devel "1.12.1"]
                                 [ring/ring-mock "0.4.0"]]
                  :plugins      [[com.jakemccrary/lein-test-refresh "0.25.0"]
                                 [jonase/eastwood "1.4.2"]] 
                  
                  :source-paths ["env/dev/clj" ]
                  :resource-paths ["env/dev/resources"]
                  :repl-options {:init-ns user
                                 :timeout 120000}
                  :injections [(require 'pjstadig.humane-test-output)
                               (pjstadig.humane-test-output/activate!)]}
   :project/test {:jvm-opts ["-Dconf=test-config.edn" ]
                  :resource-paths ["env/test/resources"] }
   :profiles/dev {}
   :profiles/test {}})
