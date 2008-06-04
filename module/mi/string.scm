(define-module mi.string (export-all))
(select-module mi.string)

; =trim
; -------------------------------------------
(define (trim str)
  (if (string=? str "") str
    ((#/^\s*(.+?)\s*$/ str) 1)
    )
  )

; =n-class-integer
; -------------------------------------------
(define (n-class-integer n i)
  (let1 c ","
    (format #f #`"~,|n|,|c|'0d" i)
    )
  )

(provide "mi/string")
