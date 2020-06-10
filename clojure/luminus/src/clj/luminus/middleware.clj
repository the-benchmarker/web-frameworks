(ns luminus.middleware
  (:require
    [luminus.env :refer [defaults]]
    [cheshire.generate :as cheshire]
    [cognitect.transit :as transit]
    [clojure.tools.logging :as log]
    [luminus.layout :refer [error-page]]
    [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
    [luminus.middleware.formats :as formats]
    [muuntaja.middleware :refer [wrap-format wrap-params]]
    [luminus.config :refer [env]]
    [ring-ttl-session.core :refer [ttl-memory-store]]
    [ring.middleware.defaults :refer [site-defaults wrap-defaults]])
  )

(defn wrap-internal-error [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable t
        (log/error t (.getMessage t))
        (error-page {:status 500
                     :title "Something very bad has happened!"
                     :message "We've dispatched a team of highly trained gnomes to take care of the problem."})))))

(defn wrap-csrf [handler]
  (wrap-anti-forgery
    handler
    {:error-response
     (error-page
       {:status 403
        :title "Invalid anti-forgery token"})}))


(defn wrap-formats [handler]
  (let [wrapped (-> handler wrap-params (wrap-format formats/instance))]
    (fn [request]
      ;; disable wrap-formats for websockets
      ;; since they're not compatible with this middleware
      ((if (:websocket? request) handler wrapped) request))))

(defn wrap-base [handler]
  (-> ((:middleware defaults) handler)
      (wrap-defaults
        (-> site-defaults
            (assoc-in [:security :anti-forgery] false)
            (assoc-in  [:session :store] (ttl-memory-store (* 60 30)))))
      wrap-internal-error))
