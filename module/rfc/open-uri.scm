(define-module rfc.open-uri
  (use rfc.http)
  (use rfc.uri)
  (export-all)
  )
(select-module rfc.open-uri)

(define-class <uri> ()
  ((uri :init-keyword :uri :init-value "")
   (scheme) (user-info) (hostname) (port) (path) (query) (fragment)
   (server) (request)
   )
  )
(define-method initialize ((self <uri>) init-args)
  (next-method)
  (receive (scheme user-info hostname port path query fragment)
    (uri-parse (ref self 'uri))
    (set! (ref self'scheme) scheme)
    (set! (ref self'user-info) user-info)
    (set! (ref self'hostname) hostname)
    (set! (ref self'port) port)
    (set! (ref self'path) path)
    (set! (ref self'query) query)
    (set! (ref self'fragment) fragment)
    (set! (ref self'server) (string-append hostname (if port #`":,port" "")))
    (set! (ref self'request) (string-append (if path path "/") (if query #`"?,query" "")))
    )
  )

(define-method http-get ((self <uri>))
  (http-get (ref self'server) (ref self'request))
  )

(define-method http-get ((uri <string>))
  (http-get (make <uri> :uri uri))
  )

(define-method http-post ((self <uri>) post-data)
  (http-post (ref self'server) (ref self'request) post-data)
  )

(define-method http-post ((uri <string>))
  (http-post (make <uri> :uri uri))
  )