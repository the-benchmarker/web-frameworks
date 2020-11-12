(ns server
  (:require
   [clojure.tools.logging :refer :all]
   [yada.yada :refer [listener resource as-resource]]))

(def app
  ["/" (resource
        {:methods
         {:get
          {:produces "text/plain"
           :response ""}}})
   [["user" (resource
             {:methods
              {:post
               {:produces "text/plain"
                :response ""}}})
     [[["/" :name] (resource
                    {:parameters {:path {:name String}}
                     :produces "text/plain"
                     :methods
                     {:get
                      {:response
                       (fn [ctx]
                         (let [name (get-in ctx [:parameters :path :name])]
                           name))}}})]]]
    [true (as-resource nil)]]])

(defn -main [& [port]]
  (listener app {:port port}))
