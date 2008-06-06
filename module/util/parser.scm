(define-module util.parser
  (export-all)
  )
(select-module util.parser)

; =key-value-parse
; '(key1 value1 key2 value2)
;   => (list :key1 value1 :key2 value2)
(define (key-value-parse ls)
  (define (kvp-origin ls res)
    (match ls
      [() res ]
      [(key value . nokori)
       (kvp-origin nokori (append res (list (make-keyword key) value)))
       ]
      [(nanikore)
       (error #`"parse error: unknown param => ,|nanikore|")
       ]
      )
    )
  (kvp-origin ls '())
  )

(provide "util/parser")
