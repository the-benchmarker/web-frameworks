(ns server
  (:require
   [clojure.tools.logging :refer :all]
   [yada.yada :as yada]))

(def index
  (yada/resource
    {:methods
     {:get
      {:produces "text/plain"
       :response ""}}}))

(def user
  (yada/resource
    {:methods
     {:post
      {:produces "text/plain"
       :response ""}}}))

(def username
  (yada/resource
    {:methods
     {:get
      {:produces "text/plain"
       :parameters {:path {:name String}}
       :response
       (fn [ctx]
         (let [name (get-in ctx [:parameters :path :name])]
           name))}}}))

(def app
  ["/"
   [["" index]
    [["/user"
      [["" user]
       ["/" :name] username]]]]]) 

(defn -main [& [port]]
  (yada/listener app {:port (Integer/parseInt port)}))
