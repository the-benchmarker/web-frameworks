(ns server
  "Production-grade Coast web server with security best practices."
  (:require [coast]
           [clojure.tools.logging :as log])
  (:gen-class))

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
;; Error Handling
;; ============================================================================

(defn- log-request
  "Log incoming requests for security auditing."
  [{:keys [request-method uri]}]
  (log/debugf "Request: %s %s" request-method uri))

(defn- handle-error
  "Handle exceptions and return appropriate error responses."
  [^Exception e request]
  (log/errorf "Error handling request %s %s: %s"
              (:request-method request) (:uri request) (.getMessage e))
  (coast/render :text "Internal Server Error" :status 500))

(defn- wrap-error-handling
  "Middleware to catch and handle exceptions."
  [handler]
  (fn [request]
    (try
      (log-request request)
      (handler request)
      (catch Exception e
        (handle-error e request)))))

(defn- wrap-security-headers
  "Middleware to add security headers to all responses."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if (map? response)
        (update response :headers (fnil into {}) security-headers)
        response))))

;; ============================================================================
;; Route Handlers
;; ============================================================================

(defn root
  "Handle GET / request."
  [_]
  (coast/render :text ""))

(defn user
  "Handle GET /user/:id request. Extracts and returns user ID from path params."
  [request]
  (let [user-id (-> request :params :id)]
    (log/debugf "Fetching user with id: %s" user-id)
    (coast/render :text user-id)))

(defn post-user
  "Handle POST /user request."
  [_]
  (coast/render :text ""))

;; ============================================================================
;; Application Setup
;; ============================================================================

(def routes
  "Application route definitions."
  [[:get "/" root]
   [:get "/user/:id" user]
   [:post "/user" post-user]])

(def app
  "Production-ready application with error handling and security middleware."
  (-> (coast/app routes)
      (coast/body-parser)
      (wrap-error-handling)
      (wrap-security-headers)))

;; ============================================================================
;; Server Lifecycle
;; ============================================================================

(defn -main
  "Server entry point. Starts the Coast server on the specified port.

  Args:
    port - Optional port number (defaults to 3000)

  JVM Options Recommended:
    -Xms256m -Xmx512m -XX:+UseG1GC -XX:MaxGCPauseMillis=200
    -Dclojure.compiler.direct-linking=true -Dcoast.env=production"
  [& [port]]
  (let [port (or port default-port)]
    (log/info "Starting Coast server on port" port)
    
    ;; Configure logging for production
    (when-not (System/getProperty "coast.env")
      (System/setProperty "coast.env" "production"))
    
    (try
      (coast/server app {:port port})
      (catch Exception e
        (log/error "Failed to start server:" (.getMessage e))
        (System/exit 1)))))
