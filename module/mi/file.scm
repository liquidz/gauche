(define-module mi.file (export-all))
(select-module mi.file)

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

(provide "mi/file")
