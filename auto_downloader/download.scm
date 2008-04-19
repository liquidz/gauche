(use mi.generic)
(use mi.abbrev)
(use mi.http)
(use gauche.process)

; -----
; todo
; * search iframe automatically
; * referer auto complete => wget --referer ***

(define (string-pickup fn target ls)
  (let1 res (fn target)
    (if res
      (my-find fn (string-scan target res 'after) (cons res ls))
      (reverse ls)
      )
    )
  )



(define (search-from-uri fn uri)
  (define (__pickup line server ls)
    (let1 res (fn line server)
      (if res
        (__pickup (string-scan line res 'after) server (cons res ls))
        (reverse ls)
        )
      )
    )

  (receive (body server req) (open-uri uri)
    (fold (lambda (line ls)
            (let1 res (__pickup line server '())
              (if (null? res) ls (append ls res))
              )
            )
          '() (split body #\newline))
    )
  )

(define (get-filename-from-uri uri)
  (car (reverse (split uri "/")))
  )

(define (print-each ls)
  (for-each (lambda (x) (print x)) ls)
  )

(define (main args)
  (print-each
    (search-from-uri (lambda (line server)
                       (let1 res ((string->regexp #`"(http://,|server|.+?)\"") line)
                         (if res (res 1) '())
                         )
                       ) "http://www.babypink.to/main.htm")
    )
  )

#|
(define (main args)
  (if (= 3 (length args))
    (cond
      [(== (cadr args) "ok")
       (for-each (lambda (archive)
                   (let1 name (get-filename-from-uri archive)
                     (process-output->string #`"wget -O ,name ,archive")
                     )
                   ) (search-from-uri (caddr args) #/(http.+?\.zip|http.+?\.lzh)/))

       ]
      [(== (cadr args) "check")
       (for-each (lambda (archive)
                   (print archive)
                   ) (search-from-uri (caddr args) #/(http.+?\.zip|http.+?\.lzh)/))
       ]
      )
    )
  )
|#
