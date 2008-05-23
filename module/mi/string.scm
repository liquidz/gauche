(define-module mi.string (export-all))
(select-module mi.string)

; =trim
; -------------------------------------------
(define (trim str)
  (if (string=? str "") str
    ((#/^\s*(.+?)\s*$/ str) 1)
    )
  )


(provide "mi/string")
