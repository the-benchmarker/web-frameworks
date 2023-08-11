(ns the-benchmarker-pedestal.service
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.body-params :as body-params]))

(def empty-response
  {:status  200
   :headers {"content-type" "text/plain"}
   :body    ""})

(defn success-response
  [body]
  {:status  200
   :headers {"content-type" "text/plain"}
   :body    body})

(defn user
  [_]
  empty-response)

(defn home-page
  [_]
  empty-response)

(defn get-user
  [request]
  (let [body (-> request
                 :path-params
                 :id)]
    (success-response body)))

(def common-interceptors [(body-params/body-params) http/html-body])

(def routes #{["/" :get (conj common-interceptors `home-page)]
              ["/user" :post (conj common-interceptors `user)]
              ["/user/:id" :get (conj common-interceptors `get-user)]})

(def service {:env :prod
              ::http/routes routes
              ::http/resource-path "/public"
              ::http/type :jetty
              ::http/port 8080
              ::http/container-options {:h2c? true
                                        :h2? false
                                        :ssl? false}})
