(ns luminus.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[luminus started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[luminus has shut down successfully]=-"))
   :middleware identity})
