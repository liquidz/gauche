(define-module mi.generic (export-all))
(select-module mi.generic)

; =+
; generic add function
; -------------------------------------------------------
(define-method + ((a <string>) . b)
  (string-append a (fold string-append "" (reverse b))))
(define-method + ((a <list>) . b)
  (append a (fold append '() (reverse b))))

; =to-i
; generic conversion function to integer
; --------------------------------------------------------
(define-method to-i ((target <string>))
  (fold (lambda (x res)
          (+ (* 10 res) (digit->integer x))
          ) 0 (string->list target))
  )
(define-method to-i ((target <real>))
  (floor->exact target)
  )
(define-method to-i ((target <rational>))
  (floor->exact target)
  )

; =to-f
; -------------------------------------------------------
(define-method to-f ((target <string>))
  (let1 reg (#/([0-9]+)\.([0-9]+)/ target)
    (let ((integral (to-i (reg 1))) (decimal (to-i (reg 2))))
      (exact->inexact
        (+ integral (/ decimal (expt 10 (string-length (reg 2))))))
      )
    )
  )
(define-method to-f ((target <integer>))
  (exact->inexact target)
  )


(provide "mi/generic")
