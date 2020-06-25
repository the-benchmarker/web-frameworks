(ns server
  (:require [yada.yada :refer [listener resource as-resource]]))

(def app
  ["/" (resource
        {:methods
         {:get
          {:produces "text/plain"
           :response ""}}})
   [
    ["user" (resource
              {:methods
               {:post
                {:produces "text/plain"
                 :response ""}}})]
    [true (as-resource nil)]]])

(defn -main [& [port]]
  (listener app {:port port}))
