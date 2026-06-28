(ns luminus.middleware
  "Production-grade middleware with security best practices."
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
    [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
    [ring.middleware.params :refer [wrap-params]]
    [ring.middleware.keyword-params :refer [wrap-keyword-params]]
    [ring.middleware.cookie :refer [wrap-cookies]])
  (:import (java.util UUID)))

;; ============================================================================
;; Security Constants
;; ============================================================================

(def ^:private security-headers
  "Standard security headers for all responses."
  {"X-Content-Type-Options"   "nosniff"
   "X-Frame-Options"          "DENY"
   "X-XSS-Protection"         "1; mode=block"
   "Strict-Transport-Security" "max-age=31536000; includeSubDomains"
   "Content-Security-Policy" "default-src 'self'; frame-ancestors 'none'"
   "Referrer-Policy"          "strict-origin-when-cross-origin"
   "Permissions-Policy"      "geolocation=(), microphone=(), camera=(), payment=()"})

(def ^:private request-id-header "X-Request-ID")

;; ============================================================================
;; Request Tracking
;; ============================================================================

(defn- generate-request-id
  "Generate a unique request ID for tracing."
  []
  (str (UUID/randomUUID)))

(defn wrap-request-id
  "Add unique request ID to each request for tracing purposes."
  [handler]
  (fn [request]
    (let [request-id (generate-request-id)]
      (-> request
          (assoc :request-id request-id)
          (assoc-in [:headers request-id-header] request-id)
          handler
          (update :headers #(into % {request-id-header request-id}))))))

;; ============================================================================
;; Security Middleware
;; ============================================================================

(defn wrap-security-headers
  "Add security headers to all responses."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if (map? response)
        (update response :headers (fnil into {}) security-headers)
        response))))

(defn wrap-disable-http-method-override
  "Prevent HTTP method override attacks."
  [handler]
  (fn [request]
    (let [request (-> request
                       (update :headers dissoc "x-http-method-override")
                       (update :headers dissoc "x-http-method")
                       (update :headers dissoc "x-method-override"))]
      (handler request))))

;; ============================================================================
;; Error Handling
;; ============================================================================

(defn wrap-internal-error
  "Handle internal server errors with proper logging and error pages."
  [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable t
        (let [request-id (or (:request-id req) "unknown")
              error-id (str "ERR-" (generate-request-id))]
          (log/errorf "[%s] Internal server error: %s - %s"
                      error-id 
                      (or (:uri req) "unknown")
                      (.getMessage t))
          (log/error t)
          
          ;; Return error response with error ID for tracing
          (error-page {:status 500
                       :title "Internal Server Error"
                       :message (str "An error occurred. Reference: " error-id)})))))))

(defn wrap-404-handling
  "Handle 404 errors with custom error page."
  [handler]
  (fn [req]
    (let [response (handler req)]
      (if (and (map? response) (= 404 (:status response)))
        (error-page {:status 404
                     :title "Page Not Found"
                     :message "The requested resource was not found."})
        response))))

(defn wrap-csrf
  "CSRF protection middleware."
  [handler]
  (wrap-anti-forgery
    handler
    {:error-response
     (error-page
       {:status 403
        :title "Forbidden"
        :message "Invalid anti-forgery token."})}))


(defn wrap-formats
  "Content negotiation and format handling middleware."
  [handler]
  (let [wrapped (-> handler wrap-params (wrap-format formats/instance))]
    (fn [request]
      ;; Disable wrap-formats for websockets
      ;; since they're not compatible with this middleware
      ((if (:websocket? request) handler wrapped) request))))

;; ============================================================================
;; Base Middleware Stack
;; ============================================================================

(defn wrap-base
  "Construct the base middleware stack with production security settings."
  [handler]
  (let [production? (-> env :prod boolean)
        session-config (-> site-defaults
                          (assoc-in [:security :anti-forgery] false)
                          (assoc-in [:session :store] (ttl-memory-store (* 60 30)))
                          (assoc-in [:session :cookie-attrs :http-only] true)
                          (assoc-in [:session :cookie-attrs :secure] production?)
                          (assoc-in [:session :cookie-attrs :same-site] :lax))]
    
    (-> ((:middleware defaults) handler)
        (cond-> production? (-> wrap-disable-http-method-override
                                   wrap-request-id
                                   wrap-security-headers))
        (wrap-defaults session-config)
        wrap-internal-error
        wrap-404-handling)))

;; ============================================================================
;; Production Optimizations
;; ============================================================================

(defn wrap-disable-debug
  "Remove debug information from responses in production."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (cond-> response
        (map? response) (-> (update :headers dissoc "Server")
                            (update :headers dissoc "X-Powered-By")
                            (update :headers dissoc "X-AspNet-Version"))))))
