(ns d.server
  (:require [com.appsflyer.donkey.core :as donkey-core]
            [com.appsflyer.donkey.server :as donkey-server])
  (:gen-class)
  (:import (io.vertx.core.impl.cpu CpuCoreSensor)))

(def empty-body
  (bytes (byte-array (map (comp byte int) ""))))

(def empty-response
  {:status  200
   :headers {"content-type" "text/plain"}
   :body    empty-body})

(defn just-id [req]
  (bytes (byte-array (map (comp byte int) (-> :path-params req (get "id"))))))

(defn get-user-id-response [req]
  {:status  200
   :headers {"content-type" "text/plain"}
   :body    (just-id req)})

(defn- get-root-handler [_ res _]
  (res empty-response))

(defn- get-user-id-handler [req res _]
  (res (get-user-id-response req)))

(defn- post-user-handler [_ res _]
  (res empty-response))

(def get-root-route {:methods [:get]
                               :path    "/"
                               :handler get-root-handler})

(def get-user-id-route {:methods [:get]
                                  :path    "/user/:id"
                                  :handler get-user-id-handler})

(def post-user-route {:methods [:post]
                                :path    "/user"
                                :handler post-user-handler})

(defn -main [& _]
  (let [concurrency (max 1 (- (CpuCoreSensor/availableProcessors) 1))]
    (->
     (donkey-core/create-donkey
      {:event-loops concurrency})
     (donkey-core/create-server {:port          3000
                                 :routes        [get-root-route get-user-id-route post-user-route]
                                 :instances     concurrency
                                 :compression   false
                                 :decompression false
                                 :date-header   true
                                 :server-header true
                                 :keep-alive    true})
     (donkey-server/start-sync))))