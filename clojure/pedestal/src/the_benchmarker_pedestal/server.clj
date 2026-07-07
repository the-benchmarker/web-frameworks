(ns the-benchmarker-pedestal.server
  "Production-grade Pedestal server with enhanced error handling and logging."
  (:gen-class)
  (:require [io.pedestal.http :as server]
            [the-benchmarker-pedestal.service :as service]
            [clojure.tools.logging :as log]))

;; ============================================================================
;; Server Lifecycle Management
;; ============================================================================

(defonce ^:private server-atom (atom nil))

defonce runnable-service
  "Create and memoize the server instance."
  (server/create-server service/service))

(defn- log-startup-info
  "Log server startup information."
  [port]
  (log/info "\n" 
            "========================================\n" 
            "  PEDESTAL SERVER STARTED              \n" 
            "========================================\n" 
            (str "  Port:       " port "             \n") 
            "  Environment: Production               \n" 
            "  Security:   Enabled                   \n" 
            "  Logging:    Configured                \n" 
            "========================================"))

(defn- log-shutdown-info
  "Log server shutdown information."
  []
  (log/info "\n" 
            "========================================\n" 
            "  PEDESTAL SERVER STOPPED              \n" 
            "========================================"))

;; ============================================================================
;; Error Handling
;; ============================================================================

(defn- handle-startup-error
  "Handle server startup errors."
  [^Exception e]
  (log/error "Failed to start Pedestal server:" (.getMessage e))
  (log/error e)
  (System/exit 1))

;; ============================================================================
;; Server Control Functions
;; ============================================================================

(defn start-server
  "Start the Pedestal server on the specified port."
  [& [port]]
  (let [port (or port 3000)]
    (try
      (log/info "Starting Pedestal server on port" port)
      
      ;; Configure production environment
      (when-not (System/getProperty "pedestal.env")
        (System/setProperty "pedestal.env" "production"))
      
      (let [server-instance (server/start runnable-service {:port port})]
        (reset! server-atom server-instance)
        (log-startup-info port)
        server-instance)
      
      (catch Exception e
        (handle-startup-error e)))))

(defn stop-server
  "Stop the running server instance."
  []
  (when-let [server @server-atom]
    (try
      (log/info "Stopping Pedestal server...")
      (server/stop server)
      (reset! server-atom nil)
      (log-shutdown-info)
      (catch Exception e
        (log/error "Error stopping server:" (.getMessage e))
        (log/error e)))))

(defn restart-server
  "Restart the server."
  [& [port]]
  (stop-server)
  (start-server port))

;; ============================================================================
;; Main Entry Point
;; ============================================================================

(defn -main
  "The entry-point for 'lein run'.

  JVM Options Recommended:
    -Xms256m -Xmx512m -XX:+UseG1GC -XX:MaxGCPauseMillis=200
    -Dpedestal.env=production -Dclojure.compiler.direct-linking=true

  Usage:
    lein run [port]
    java -jar target/uberjar/the-benchmarker-pedestal-standalone.jar [port]"
  [& args]
  (let [port (when (seq args) (Integer/parseInt (first args)))]
    (start-server port)
    
    ;; Add shutdown hook for graceful shutdown
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop-server))))
