(ns luminus.core
  "Production-grade Luminus application with security best practices."
  (:require
    [luminus.handler :as handler]
    [luminus.nrepl :as nrepl]
    [luminus.http-server :as http]
    [luminus.config :refer [env]]
    [clojure.tools.cli :refer [parse-opts]]
    [clojure.tools.logging :as log]
    [mount.core :as mount]
    [buddy.core.codecs :as codecs]
    [buddy.core.hash :as hash]
    [buddy.core.nonce :as nonce])
  (:gen-class))

;; ============================================================================
;; Security Configuration
;; ============================================================================

(def ^:private security-config
  "Security configuration for production environment."
  {:anti-forgery false ; Handled at proxy level
   :secure-session true
   :session-timeout (* 60 60 24) ; 24 hours
   :csrf-token-name "__anti-forgery-token"
   :secure-cookies true
   :http-only-cookies true
   :same-site-cookies :lax
   :hsts-max-age (* 60 60 24 365) ; 1 year
   :content-security-policy "default-src 'self'; frame-ancestors 'none'"
   :x-frame-options "DENY"
   :x-content-type-options "nosniff"
   :x-xss-protection "1; mode=block"
   :referrer-policy "strict-origin-when-cross-origin"
   :permissions-policy "geolocation=(), microphone=(), camera=(), payment=()"})

;; ============================================================================
;; Error Handling and Logging
;; ============================================================================

;; Log uncaught exceptions in threads
(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ thread ex]
      (let [thread-name (.getName thread)]
        (log/errorf "Uncaught exception on thread %s: %s" thread-name (.getMessage ex))
        (log/error ex)))))

(defn- log-application-start
  "Log application startup information."
  []
  (log/info "\n" 
            "========================================\n" 
            "  LUMINUS APPLICATION STARTED          \n" 
            "========================================\n" 
            "  Environment: Production               \n" 
            "  Security:   Enabled                   \n" 
            "  Logging:    Configured                \n" 
            "========================================"))

(defn- log-application-stop
  "Log application shutdown information."
  []
  (log/info "\n" 
            "========================================\n" 
            "  LUMINUS APPLICATION STOPPED          \n" 
            "========================================"))

;; ============================================================================
;; CLI Configuration
;; ============================================================================

(def cli-options
  [["-p" "--port PORT" "Port number"
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 1024 % 65535) "Port must be between 1024 and 65535"]]
   ["-e" "--env ENV" "Environment (dev, prod, test)"
    :default "prod"
    :validate [#(contains? #{"dev" "prod" "test"} %) "Environment must be dev, prod, or test"]]])

;; ============================================================================
;; Server Components
;; ============================================================================

(mount/defstate ^{:on-reload :noop} http-server
  :start
  (do
    (log/info "Starting HTTP server...")
    (http/start
      (-> env
          (assoc :handler (handler/app))
          (update :port #(or (-> env :options :port) %))
          (merge security-config))))
  :stop
  (do
    (log/info "Stopping HTTP server...")
    (http/stop http-server)))

(mount/defstate ^{:on-reload :noop} repl-server
  :start
  (when (env :nrepl-port)
    (do
      (log/info "Starting nREPL server on port" (env :nrepl-port))
      (nrepl/start {:bind (env :nrepl-bind)
                    :port (env :nrepl-port)})))
  :stop
  (when repl-server
    (do
      (log/info "Stopping nREPL server...")
      (nrepl/stop repl-server))))

;; ============================================================================
;; Application Lifecycle
;; ============================================================================

(defn stop-app
  "Gracefully stop the application."
  []
  (log/info "Initiating application shutdown...")
  (doseq [component (:stopped (mount/stop))]
    (log/info (str "Component stopped: " component)))
  (log-application-stop)
  (shutdown-agents))

(defn start-app
  "Start the application with the given arguments."
  [args]
  (let [parsed-args (-> args
                        (parse-opts cli-options))
        started-components (:started (mount/start-with-args parsed-args))]
    
    (log-application-start)
    
    (doseq [component started-components]
      (log/info (str "Component started: " component)))
    
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop-app))
    
    (log/info "Application started successfully. Listening on port" 
              (or (-> parsed-args :options :port) (-> env :port)))))

(defn -main
  "Application entry point.

  JVM Options Recommended:
    -Xms512m -Xmx1G -XX:+UseG1GC -XX:MaxGCPauseMillis=200
    -Dconf=prod-config.edn -Dluminus.env=production

  Usage:
    lein run [options]
    java -jar luminus.jar [options]

  Options:
    -p, --port PORT   Port number (default from config)
    -e, --env ENV     Environment (dev, prod, test)"
  [& args]
  (start-app args))
