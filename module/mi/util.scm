(define-module mi.util (export-all))
(select-module mi.util)

; =hash
; macro for hash-table
; -----------------------------------------------------
(define-syntax hash
  (syntax-rules ()
	[(hash)
	 (make-hash-table)
	 ]
	[(hash table)
	 (define table (make-hash-table))
	 ]
	[(hash table key)
	 (hash-table-get table key)
	 ]
	[(hash table key value)
	 (hash-table-put! table key value)
	 ]
	))


; =counter
; --------------------------------------------------
(define (counter)
  (let1 count 0
    (lambda ()
      (let1 return count
        (set! count (+ count 1))
        return
        )
      )
    )
  )


(provide "mi/util")
