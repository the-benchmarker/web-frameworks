(ns luminus.env
  (:require
    [selmer.parser :as parser]
    [clojure.tools.logging :as log]
    [luminus.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[luminus started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[luminus has shut down successfully]=-"))
   :middleware wrap-dev})
