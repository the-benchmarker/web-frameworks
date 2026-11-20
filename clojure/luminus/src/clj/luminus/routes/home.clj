(ns luminus.routes.home
  (:require
   [clojure.java.io :as io]
   [luminus.middleware :as middleware]
   [ring.util.response]
   [ring.util.http-response :as response]))

(defn index-page [request]
  {:body ""})

(defn user-page [{:keys [path-params query-params body-params]}]
  {:body (:id path-params) })

(defn user-post [request]
  {:body ""})    

(defn home-routes []
  [""
   {:middleware [middleware/wrap-formats]}
   ["/" {:get index-page}]
   ["/user" {:post user-post}]
   ["/user/:id" {:get user-page}]])

