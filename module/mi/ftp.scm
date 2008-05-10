(define-module mi.ftp
  (use rfc.ftp)
  (export-all))
(select-module mi.ftp)

(define (or-null? . ls)
  (call/cc (lambda (cc)
             (fold (lambda (x res)
                     (if (null? x) (cc #t) #f)
                     ) #f ls)
             )
           )
  )

(define (upload-files . args)
  (define (upload-to-a-directory con args)
    (let-keywords args ((dir '()) (file '()) (mode 'ascii))
      (unless (or-null? dir file)
        (guard (e (else (print "error occur") (exit)))
          (set! (ftp-transfer-type con) mode)
          (ftp-chdir con dir)
          (for-each (lambda (x)
                      (ftp-put con x)
                      )
                    (if (list? file) file (list file)))
          )
        )
      )
    )

  (let-keywords args ((host '()) (user '())
                                 (password '()) (passive #f)
                                 (files '())
                                 )
    (if (or-null? host user password files) #f
      (call-with-ftp-connection
        host (lambda (con)
               (for-each (lambda (x)
                           (upload-to-a-directory con x)) files)
               )
        :passive passive
        :username user
        :password password
        )
      )
    )
  )


(provide "mi/ftp")
