(ns server
  (:gen-class)
  (:require
   [clojure.tools.logging :refer :all]
   [yada.yada :as yada]))

(def index
  (yada/resource
    {:methods
     {:get
      {:produces "text/plain"
       :response ""}}}))

(def postuser
  (yada/resource
    {:methods
     {:post
      {:produces "text/plain"
       :response ""}}}))

(def getuser
  (yada/resource
    {:parameters {:path {:name String}}
     :methods
     {:get
      {:produces "text/plain"
       :response
       (fn [ctx]
         (let [name (get-in ctx [:parameters :path :name])]
           name))}}}))

(def app
  ["/"
   [["" index]
    ["user"
      [["" postuser]
       [["/" :name] getuser]]]]]) 

(defn -main [& [port]]
  (yada/listener app {:port (Integer/parseInt port)}))
