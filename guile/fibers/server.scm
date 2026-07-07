(define-module (server) #:export (main))

;; Import required modules for production-grade web server
(import (web request) (web response) (web uri) 
        (fibers) (fibers web server) 
        (ice-9 match) (srfi srfi-1) (srfi srfi-9))

;;; Security headers configuration
;; Production-grade security headers for all responses
(define security-headers
  `((x-content-type-options . "nosniff")
    (x-frame-options . "DENY")
    (x-xss-protection . "1; mode=block")
    (content-security-policy . "default-src 'self'")
    (referrer-policy . "strict-origin-when-cross-origin")
    (permissions-policy . "geolocation=(), microphone=(), camera=()")))

;;; Configuration constants
(define default-port 3000)
(define max-body-size (* 16 1024 1024))  ;; 16 MB
(define read-timeout 30)  ;; 30 seconds
(define write-timeout 30)  ;; 30 seconds
(define idle-timeout 120)  ;; 120 seconds

;;; Request handling procedures

;; add-security-headers: Add security headers to response
;; @param headers - existing headers alist
;; @returns enhanced headers alist
(define (add-security-headers headers)
  (append headers security-headers))

;; root-handler: Handle requests to the root endpoint
;; @param request - HTTP request object
;; @param body - request body
;; @returns response and body
(define (root-handler request body)
  (values
   (build-response
    #:headers (add-security-headers `((content-type . (text/plain))))
    #:code 200)
   ""))

;; get-user-handler: Handle GET requests to /user/:id
;; @param request - HTTP request object
;; @param body - request body
;; @returns response and body
(define (get-user-handler request body)
  (define path (uri-path (request-uri request)))
  (define user-prefix "/user/")
  (define user-id (string-drop path (string-length user-prefix)))
  (values
   (build-response
    #:headers (add-security-headers `((content-type . (text/plain))))
    #:code 200)
   user-id))

;; create-user-handler: Handle POST requests to /user
;; @param request - HTTP request object
;; @param body - request body
;; @returns response and body
(define (create-user-handler request body)
  (values
   (build-response
    #:headers (add-security-headers `((content-type . (text/plain))))
    #:code 200)
   ""))

;; health-check-handler: Handle health check requests
;; @param request - HTTP request object
;; @param body - request body
;; @returns response and body
(define (health-check-handler request body)
  (values
   (build-response
    #:headers (add-security-headers 
              `((content-type . (text/plain))
                (cache-control . "no-cache, no-store, must-revalidate")))
    #:code 200)
   "OK"))

;; not-found-handler: Handle 404 Not Found
;; @param request - HTTP request object
;; @param body - request body
;; @returns response and body
(define (not-found-handler request body)
  (values
   (build-response
    #:headers (add-security-headers `((content-type . (text/plain))))
    #:code 404)
   "Not Found"))

;;; Main request dispatcher
(define (dispatch-request request body)
  (define path (uri-path (request-uri request)))
  (define method (request-method request))
  
  (cond
   ;; Root endpoint
   ((string=? path "/") (root-handler request body))
   
   ;; Health check endpoint
   ((string=? path "/health") (health-check-handler request body))
   
   ;; User endpoints
   ((and (string-prefix? "/user/" path) (equal? method 'GET))
    (get-user-handler request body))
   
   ((and (string=? "/user" path) (equal? method 'POST))
    (create-user-handler request body))
   
   ;; 404 Not Found
   (else (not-found-handler request body))))

;;; Server configuration and startup

;; get-port: Get port from environment or use default
;; @returns port number
(define (get-port)
  (let ((port-str (getenv "PORT")))
    (if (and port-str (string->number port-str))
        (string->number port-str)
        default-port)))

;; configure-server: Configure production-grade Fibers server
;; @returns configured server
(define (configure-server)
  (let ((server (make <fibers-web-server>)))
    ;; Set server configuration
    (slot-set! server 'family AF_INET)
    (slot-set! server 'addr 0)
    (slot-set! server 'port (get-port))
    
    ;; Production configuration - disable debug features
    (slot-set! server 'debug? #f)
    (slot-set! server 'log-level 'error)  ;; Only error logging in production
    (slot-set! server 'max-body-size max-body-size)
    
    ;; Set request handler
    (slot-set! server 'handler dispatch-request)
    
    server))

;;; Main entry point
(define (main args)
  ;; Configure production-grade server
  (let ((server (configure-server)))
    
    ;; Start server with error handling
    (catch 'system-error
      (lambda ()
        (format (current-error-port) "Starting Fibers benchmark server on port ~a~%" 
                (slot-ref server 'port))
        (run server))
      
      (lambda (key . args)
        (format (current-error-port) "Server error: ~a~%" args)
        (exit 1)))))
