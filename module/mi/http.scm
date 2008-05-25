(define-module mi.http
  (use rfc.uri)
  (use rfc.http)
  (use rfc.base64)
  (use gauche.charconv)
  (export open-uri))
(select-module mi.http)

(define (make-auth-list user/pass)
  (if (string=? user/pass "") '()
    (let1 enc-user/pass (base64-encode-string user/pass)
      (list :Authorization #`"Basic ,enc-user/pass")
      )
    )
  )

(define (make-authentication-string user password)
  (let1 encoded-user-pass (base64-encode-string #~",|user|:,|password|")
    #`"Basic ,encoded-user-pass"
    )
  )

(define (get-server-request-from-uri uri)
  (receive (scheme user-info hostname port path query frag) (uri-parse uri)
    (let ((server (string-append hostname (if port #`":,port" "")))
          (request (string-append path (if query #`"?,query" ""))))
      (values server request)
      )
    )
  )

(define (open-uri uri . args)
  (receive (server request) (get-server-request-from-uri uri)
    (let-keywords args ((method 'get)
                        (data '())
                        (user "")
                        (password "")
                        )
      )
    )
  (receive (scheme user-info hostname port path query frag) (uri-parse uri)
    (let ((server (string-append hostname (if port #`":,port" "")))
          (request (string-append path (if query #`"?,query" ""))))
      (let1 fn (if (null? (car arg)) (delay (http-get server request))
                 (delay (http-post server request (car arg)))
                 )
        (receive (status header body) (force fn)
          (values body status header)
          )
      )
    )
  )

#;(define (open-uri uri . options)
  (receive (scheme user-info hostname port path query frag) (uri-parse uri)
    (let ((server (if port #`",hostname:,port" #`",hostname"))
          (request (if query #`",|path|?,query" #`",path")))
      (let-keywords options ((method 'get) (user/pass ""))
        (let ((get-args (delay (append (list server request)
                                      (make-auth-list user/pass))))
              (post-args (delay (append (list server path query)
                                       (make-auth-list user/pass)))))
          (receive (status header body)
            (if (eq? method 'post)
              (apply http-post (force post-args))
              (apply http-get (force get-args))
              )
            (values (ces-convert body "*JP") server request)
            )
          )
        )
      )
    )
  )

(provide "mi/http")
