(define-module mi.abrev
  (use srfi-1)
  (export-all))
(select-module mi.abrev)

(define r reverse)
(define uniq delete-duplicates)
(define uniq! delete-duplicates!)
(define split string-split)

(define (++ i) (+ i 1))
(define (-- i) (- i 1))

(provide "mi/abbrev")
