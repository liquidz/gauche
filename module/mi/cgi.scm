(define-module mi.cgi
  (use text.html-lite)
  (use www.cgi)
  (export-all)
  )
(select-module mi.cgi)

#|
(define (cgi . args)
  (let-keywords args
    [() (cgi-parse-parameters)]
    []
    )
  )
|#


(provide "mi/cgi")
