(ns d.server
  "Production-grade Donkey web server with security best practices."
  (:require [com.appsflyer.donkey.core :as donkey-core]
            [com.appsflyer.donkey.server :as donkey-server]
            [clojure.tools.logging :as log])
  (:gen-class)
  (:import (io.vertx.core.impl.cpu CpuCoreSensor)))

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
   "Permissions-Policy"      "geolocation=(), microphone=(), camera=()"})

;; ============================================================================
;; Response Utilities
;; ============================================================================

(defn- bytes
  "Convert string to bytes."
  [^String s]
  (-> s (.getBytes "UTF-8")))

(def ^:private empty-body
  (bytes ""))

(defn- make-response
  "Create a standardized response map."
  ([status body]
   (make-response status "text/plain" body))
  ([status content-type body]
   {:status  status
    :headers {"content-type" content-type
              "Server"       "Donkey"}
    :body    (bytes body)}))

(def ^:private empty-response
  (make-response 200 ""))

(def ^:private error-response
  (make-response 500 "Internal Server Error"))

;; ============================================================================
;; Request Handlers
;; ============================================================================

(defn- get-user-id-response
  "Create response with user ID from path params."
  [req]
  (let [user-id (-> req :path-params (get "id"))]
    (log/debugf "Processing user request for id: %s" user-id)
    (make-response 200 (or user-id ""))))

(defn- get-root-handler
  "Handle GET / request."
  [_ res _]
  (res (-> empty-response
           (update :headers merge security-headers))))

(defn- get-user-id-handler
  "Handle GET /user/:id request."
  [req res _]
  (try
    (res (-> (get-user-id-response req)
             (update :headers merge security-headers)))
    (catch Exception e
      (log/errorf "Error in get-user-id-handler: %s" (.getMessage e))
      (res (-> error-response
               (update :headers merge security-headers))))))

(defn- post-user-handler
  "Handle POST /user request."
  [_ res _]
  (res (-> empty-response
           (update :headers merge security-headers))))

;; ============================================================================
;; Route Definitions
;; ============================================================================

(def ^:private get-root-route
  {:methods [:get]
   :path    "/"
   :handler get-root-handler})

(def ^:private get-user-id-route
  {:methods [:get]
   :path    "/user/:id"
   :handler get-user-id-handler})

(def ^:private post-user-route
  {:methods [:post]
   :path    "/user"
   :handler post-user-handler})

;; ============================================================================
;; Server Configuration
;; ============================================================================

(defn- create-production-server
  "Create a production-ready Donkey server with security best practices."
  [donkey-instance port]
  (let [routes [get-root-route get-user-id-route post-user-route]
        num-cores (max 1 (- (CpuCoreSensor/availableProcessors) 1))
        concurrency (max 4 num-cores)] ; Ensure minimum 4 event loops
    
    (donkey-core/create-server
     donkey-instance
     {:port          port
      :routes        routes
      :instances     concurrency
      :compression   false ; Disable to avoid CPU overhead
      :decompression false ; Disable to avoid CPU overhead
      :date-header   false ; Let application control headers
      :server-header false ; Remove server header for security
      :keep-alive    true
      :max-body-size (* 1024 1024) ; 1MB limit for request body
      
      ;; Security settings
      :ssl false ; Disable SSL at application level (use reverse proxy)
      :client-auth :none}))) ; Disable client authentication

;; ============================================================================
;; Server Lifecycle
;; ============================================================================

(defn -main
  "Server entry point. Starts the Donkey server on the specified port.

  JVM Options Recommended:
    -Xms2G -Xmx2G -XX:+UseStringDeduplication -XX:+UseParallelGC
    -Dvertx.disableMetrics=true -Dvertx.threadChecks=false
    -Dvertx.disableContextTimings=true -Dvertx.disableTCCL=true
    -Dvertx.disableH2c=true -Dvertx.disableWebsockets=true
    -Dvertx.disableHttpHeadersValidation=true -Dvertx.flashPolicyHandler=false
    -Djava.net.preferIPv4Stack=true -Ddonkey.env=production"
  [& [port]]
  (let [port (or port default-port)]
    (log/info "Starting Donkey server on port" port "with production configuration")
    
    ;; Configure production environment
    (when-not (System/getProperty "donkey.env")
      (System/setProperty "donkey.env" "production"))
    
    (try
      (let [donkey-instance (donkey-core/create-donkey
                             {:event-loops (max 4 (max 1 (- (CpuCoreSensor/availableProcessors) 1)))
                              :metrics-enabled false}) ; Disable metrics for production
            server (create-production-server donkey-instance port)]
        (log/info "Server configured with security headers and error handling")
        (donkey-server/start-sync server))
      
      (catch Exception e
        (log/error "Failed to start server:" (.getMessage e))
        (System/exit 1))))))