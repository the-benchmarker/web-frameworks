(define-module (server) #:export (main))
(import (web server) (web request) (web response) (web uri))

(define (handler request body)
  (define method (request-method request))
  (define path (uri-path (request-uri request)))
  (define userpath "/user")
  (define userpath? (string-prefix? userpath path))
  (values
   (build-response
    #:headers `((content-type . (text/plain)))
    #:code 200)
   (cond
    ((and (equal? method 'POST)
          userpath?)
     "")
    (userpath?
     (string-drop path (1+ (string-length userpath))))
    (else ""))))

(define (main args)
  (run-server handler 'http '(#:port 3000 #:addr 0)))
