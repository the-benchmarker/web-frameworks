(ns server
  (:require [coast])
  (:gen-class))


(defn root [_]
  (coast/render :text ""))


(defn user [request]
  (coast/render :text (-> request :params :id)))


(defn post-user [_]
  (coast/render :text ""))


(def routes
  [[:get "/" root]
   [:get "/user/:id" user]
   [:post "/user" post-user]])


(def app
  (-> (coast/app routes)
      (coast/body-parser)))


(defn -main [& [port]]
  (coast/server app {:port port}))
