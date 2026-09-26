(define-module (server) #:export (main))
(import (web server) (web request) (web response) (web uri))

(define (handler request body)
  (let* ((method (request-method request))
         (path (uri-path (request-uri request)))
         (user-id? (and (eq? method 'GET)
                        (string-prefix? "/user/" path)
                        (> (string-length path) 6)))
         (matched? (or user-id?
                       (and (eq? method 'GET) (string=? path "/"))
                       (and (eq? method 'POST) (string=? path "/user")))))
    (values (build-response #:code (if matched? 200 404)
                            #:headers '((content-type . (text/plain))))
            (if user-id? (substring path 6) ""))))

(define (main args)
  (run-server handler 'fibers '(#:port 3000 #:addr 0)))
