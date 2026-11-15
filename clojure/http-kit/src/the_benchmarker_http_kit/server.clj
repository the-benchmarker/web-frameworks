(ns the-benchmarker-http-kit.server
  (:gen-class)
  (:require [org.httpkit.server :as http-kit]
            [reitit.ring :as ring]))

(def empty-response
  {:status 200
   :body   ""})

(defn home-page
  [_]
  empty-response)

(defn create-user
  [_]
  empty-response)

(defn get-user
  [request]
  {:status 200
   :body   (-> request :path-params :id)})

(def app
  (ring/ring-handler
   (ring/router
    [["/" {:get home-page}]
     ["/user" {:post create-user}]
     ["/user/:id" {:get get-user}]])
   (ring/create-default-handler)))

(defn -main
  [& _]
  (http-kit/run-server app {:ip "0.0.0.0" :port 3000}))
