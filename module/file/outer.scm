(define-module file.outer
  (use gauche.interactive)
  (export
    expand-outer-scheme
    call-from-outer-scheme
    outer-scheme-fold
    )
  )
(select-module file.outer)

(define-module outer-scheme-inner-module)
(define *this-module-name* 'outer-scheme-inner-module)

(define (read-file path)
  (call-with-input-file
    (if (symbol? path) (x->string path) path)
    (lambda (port) (port->list read port)))
  )

(define (expand-outer-scheme path)
  (let1 data (read-file path)
    (for-each (lambda (s) (eval s (find-module *this-module-name*))) data)
    )
  )

(define (call-from-outer-scheme . expr)
  (fold (lambda (x res)
          (eval x (find-module *this-module-name*))
          ) '() expr)
  )

(define-syntax outer-scheme-fold
  (syntax-rules ()
    [(_ fn init ls)
     (fold (lambda (file res)
             (expand-outer-scheme file)
             (with-module outer-scheme-inner-module
               (fn file res)
               )
             ) init ls)
     ]
    )
  )


(provide "file/outer")
