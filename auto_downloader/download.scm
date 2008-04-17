(use mi.generic)
(use mi.abbrev)
(use mi.http)
(use gauche.process)

; -----
; todo
; * search iframe automatically
; * referer auto complete => wget --referer ***

(define (search-archive uri)
  (define (__get-zip-lzh line ls)
    (let1 reg (#/(http.+?\.zip|http.+?\.lzh)/ line)
      (if reg
        (__get-zip-lzh (reg 'after) (cons (reg 1) ls))
        (reverse ls)
        )
      )
    )

  (let1 src (open-uri uri)
    (fold (lambda (line ls)
            (let1 res (__get-zip-lzh line '())
              (if (null? res) ls (+ ls res))
              )
            ) '() (split src #\newline))
    )
  )

(define (get-filename-from-uri uri)
  (car (reverse (split uri "/")))
  )

(define (main args)
  (if (= 3 (length args))
	(cond
	  [(== (cadr args) "ok")
	   (for-each (lambda (archive)
				   (let1 name (get-filename-from-uri archive)
						 (process-output->string #`"wget -O ,name ,archive")
						 )
				   ) (search-archive (caddr args)))

	   ]
	  [(== (cadr args) "check")
	   (for-each (lambda (archive)
				   (print archive)
				   ) (search-archive (caddr args)))
	   ]
	  )
	)
  )
