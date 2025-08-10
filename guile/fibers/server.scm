(define-module (server) #:export (main))
(import (web request) (web response) (web uri) (fibers) (fibers web server))

(define (handler request body)
  (define method (request-method request))
  (define path (uri-path (request-uri request)))
  (define userpath "/user")
  (values
   (build-response
    #:headers `((content-type . (text/plain)))
    #:code 200)
   (cond
    ((and (equal? method 'POST)
          (string-prefix-ci? userpath path))
     "")
    ((string-prefix-ci? userpath path)
     (string-drop path (1+ (string-length userpath))))
    ((equal? "/" path)
     "")
    (else ""))))

(define (main args)
  (run-server handler #:family AF_INET #:port 3000 #:addr 0))
