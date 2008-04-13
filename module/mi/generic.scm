(define-module mi.generic
			   (export-all))
(select-module mi.generic)

(define-method + ((a <string>) . b)
  (string-append a (fold string-append "" (reverse b))))
(define-method + ((a <list>) . b)
  (append a (fold append '() (reverse b))))


(provide "mi/generic")
