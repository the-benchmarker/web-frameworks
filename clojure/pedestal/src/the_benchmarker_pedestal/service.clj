(ns the-benchmarker-pedestal.service
  "Production-grade Pedestal service with security best practices."
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.interceptor :refer [interceptor]]
            [clojure.tools.logging :as log])
  (:import (java.util UUID)))

;; ============================================================================
;; Configuration Constants
;; ============================================================================

(def ^:private default-port 3000)
(def ^:private security-headers
  {"X-Content-Type-Options"   "nosniff"
   "X-Frame-Options"          "DENY"
   "X-XSS-Protection"         "1; mode=block"
   "Strict-Transport-Security" "max-age=31536000; includeSubDomains"
   "Content-Security-Policy" "default-src 'self'; frame-ancestors 'none'"
   "Referrer-Policy"          "strict-origin-when-cross-origin"
   "Permissions-Policy"      "geolocation=(), microphone=(), camera=()"
   "Server"                 "Pedestal"})

;; ============================================================================
;; Response Utilities
;; ============================================================================

(defn- make-response
  "Create a standardized response with security headers."
  ([status body]
   (make-response status "text/plain" body))
  ([status content-type body]
   {:status  status
    :headers (into {"content-type" content-type} security-headers)
    :body    body}))

(def ^:private empty-response
  (make-response 200 ""))

(def ^:private error-response
  (make-response 500 "Internal Server Error"))

(defn success-response
  "Create a success response with the given body."
  [body]
  (make-response 200 body))

(defn not-found-response
  "Create a 404 not found response."
  []
  (make-response 404 "Not Found"))

;; ============================================================================
;; Request Utilities
;; ============================================================================

(defn- generate-request-id
  "Generate a unique request ID for tracing."
  []
  (str (UUID/randomUUID)))

(defn- add-request-id
  "Add request ID to context for tracing."
  [context]
  (let [request-id (generate-request-id)]
    (-> context
        (assoc :request-id request-id)
        (assoc-in [:response :headers "X-Request-ID"] request-id))))

;; ============================================================================
;; Request Handlers
;; ============================================================================

(defn home-page
  "Handle GET / request."
  [request]
  (log/debugf "[%s] Handling root request" (get-in request [:context :request-id]))
  empty-response)

(defn user
  "Handle POST /user request."
  [request]
  (log/debugf "[%s] Handling POST /user request" (get-in request [:context :request-id]))
  empty-response)

(defn get-user
  "Handle GET /user/:id request. Returns the user ID from path params."
  [request]
  (let [user-id (-> request :path-params :id)]
    (log/debugf "[%s] Handling GET /user/%s request" 
                (get-in request [:context :request-id]) 
                user-id)
    (success-response (or user-id ""))))

;; ============================================================================
;; Error Handling
;; ============================================================================

(defn- handle-exception
  "Handle exceptions and return appropriate error responses."
  [exception request]
  (let [error-id (str "ERR-" (generate-request-id))]
    (log/errorf "[%s] Exception: %s" error-id (.getMessage exception))
    (log/error exception)
    (-> error-response
        (assoc :body (str "Internal Server Error. Reference: " error-id))
        (assoc-in [:headers "X-Error-ID"] error-id))))

(def exception-interceptor
  "Interceptor to catch and handle exceptions."
  (interceptor
   {:name ::exception-handler
    :error (fn [context exception]
             (assoc context :response (handle-exception exception (:request context))))}))

;; ============================================================================
;; Security Interceptors
;; ============================================================================

(def request-id-interceptor
  "Interceptor to add request ID for tracing."
  (interceptor
   {:name ::request-id
    :enter add-request-id}))

(def security-headers-interceptor
  "Interceptor to add security headers to all responses."
  (interceptor
   {:name ::security-headers
    :leave (fn [context]
             (if-let [response (:response context)]
               (update context :response #(update % :headers merge security-headers))
               context))}))

;; ============================================================================
;; Route Definitions
;; ============================================================================

(def common-interceptors
  "Common interceptors for all routes."
  [(body-params/body-params)
   http/html-body
   request-id-interceptor
   security-headers-interceptor
   exception-interceptor])

(def routes
  "Application route definitions."
  #{["" :get (conj common-interceptors `home-page)]
     ["/user" :post (conj common-interceptors `user)]
     ["/user/:id" :get (conj common-interceptors `get-user)]})

;; ============================================================================
;; Service Configuration
;; ============================================================================

(def service
  "Production-ready Pedestal service configuration."
  {:env :prod
   ::http/routes routes
   ::http/resource-path "/public"
   ::http/type :jetty
   ::http/port default-port
   ::http/host "0.0.0.0"
   ::http/container-options {:h2c? true
                             :h2? false
                             :ssl? false
                             :max-threads 200
                             :min-threads 8
                             :thread-idle-timeout 60000
                             :max-queued-requests 100
                             :send-date-header? false
                             :send-server-header? false
                             :header-buffer-size 8192
                             :response-buffer-size 16384
                             :request-buffer-size 16384
                             :max-body-size (* 1024 1024)}})