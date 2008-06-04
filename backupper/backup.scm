(use file.util)
(use srfi-1)
(use srfi-19)

(define (today)
  (let1 now (current-date)
    (let ((year (date-year now)) (month (date-month now)) (day (date-day now)))
      #`",|year|-,|month|-,|day|"
      )
    )
  )

(define (make-backup-filename filename)
  (let1 ls (reverse (string-split filename "."))
    (fold string-append "" (append (list (car ls) "." (today) "-back.") (reverse (cdr ls))))
    )
  )

(define (main args)
  (for-each (lambda (filename)
              (print (make-backup-filename filename))
              (copy-file filename (make-backup-filename filename) :if-exists :backup)
              ) (cdr args))
  )
