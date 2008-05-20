(define-module mi.uses
  (export uses)
  )
(select-module mi.uses)

(define-syntax make-uses-list
  (syntax-rules ()
    [(_) '()]
    [(_ mod) (delay (use mod))]
    [(_ mod mod2 ...)
     (cons (delay (use mod)) (make-uses-list mod2 ...))
     ]
    )
  )

(define-syntax uses
  (syntax-rules ()
    [(_ mods ...)
     (for-each (lambda (m) (force m)) (make-uses-list mods ...))
     ]
    )
  )

(provide "mi/uses")
