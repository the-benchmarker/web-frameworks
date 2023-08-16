(ns the-benchmarker-pedestal.server
  (:gen-class)
  (:require [io.pedestal.http :as server]
            [the-benchmarker-pedestal.service :as service]))

(defonce runnable-service (server/create-server service/service))

(defn -main
  "The entry-point for 'lein run'"
  [& args]
  (println "\nCreating your server...")
  (server/start runnable-service))
