(ns luminus.routes.home
  "Home routes with enhanced readability and error handling."
  (:require
   [clojure.java.io :as io]
   [luminus.middleware :as middleware]
   [ring.util.response :as ring-response]
   [ring.util.http-response :as response]
   [clojure.tools.logging :as log])
  (:import (clojure.lang ExceptionInfo)))

;; ============================================================================
;; Route Handlers
;; ============================================================================

(defn index-page
  "Handle GET / request."
  [request]
  (log/debug "Handling root request")
  {:status 200
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body ""})

(defn user-page
  "Handle GET /user/:id request. Returns the user ID from path parameters."
  [{:keys [path-params query-params body-params]}]
  (let [user-id (:id path-params)]
    (log/debugf "Handling user request for id: %s" user-id)
    {:status 200
     :headers {"Content-Type" "text/plain; charset=utf-8"}
     :body (or user-id "")}))

(defn user-post
  "Handle POST /user request."
  [request]
  (log/debug "Handling POST /user request")
  {:status 201 ; 201 Created for POST success
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body ""})

;; ============================================================================
;; Route Definitions
;; ============================================================================

(defn home-routes
  "Define the home routes with appropriate middleware."
  []
  [""
   {:middleware [middleware/wrap-formats
                 middleware/wrap-request-id
                 middleware/wrap-security-headers]}
   ["/" {:get index-page
         :name ::index-page}]
   ["/user" {:post user-post
             :name ::user-post}]
   ["/user/:id" {:get user-page
                 :name ::user-page
                 :constraints {:id #".+"}}]]) ; Ensure ID is not empty

