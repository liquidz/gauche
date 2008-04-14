(define-module mi.abbrev
  (use srfi-1)
  (export-all))
(select-module mi.abbrev)

(define r reverse)
(define uniq delete-duplicates)
(define uniq! delete-duplicates!)
(define split string-split)

(define (++ i) (+ i 1))
(define (-- i) (- i 1))

(provide "mi/abbrev")
