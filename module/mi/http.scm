(define-module mi.http
  (use rfc.uri)
  (use rfc.http)
  (use gauche.charconv)
  (export-all))
(select-module mi.http)

(define (open-uri uri)
  (receive (scheme user-info hostname port path query frag) (uri-parse uri)
    (let ((server (if port #`",hostname:,port" #`",hostname"))
          (request (if query #`",path?,query" #`",path")))
      (receive (status header body) (http-get server request)
        (values (ces-convert body "*JP") server request)
        )
      )
    )
  )

(provide "mi/http")
