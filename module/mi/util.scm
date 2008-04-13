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

(provide "mi/util")
