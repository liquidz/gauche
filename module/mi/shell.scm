(define-module mi.shell
			   (use srfi-1)
			   (use mi.abbrev)
			   (use mi.util)
			   (export-all))
(select-module mi.shell)

; =read-file
; read each line from file
; (fn line count)
; ---------------------------------------------------
(define (read-file fn path)
  (with-input-from-file
    path (lambda ()
           (port-fold (lambda (line count)
						(fn line count)
						(+ count 1)
						) 1 read-line)
		   ))
  )

; =file->list
; make a list which has each line of a file
; --------------------------------------------------
(define (file->list path)
  (with-input-from-file
	path (lambda ()
		   (reverse
			 (port-fold (lambda (line ls)
						  (cons line ls)
						  ) '() read-line)
					)
		   )
	)
  )

(provide "mi/shell")
