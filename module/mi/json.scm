(define-module mi.json (export-all))
(select-module mi.json)

(define (json-array? ls)
  (if (string? (caar ls)) #f #t))

(define (make-json-pair data)
  (if (list? data)
    (string-append "\"" (car data) "\": \"" (cadr data) "\"")
    (error "this is not a list")))

(define (make-json-hash data)
  (let ((json (fold (lambda (item json)
                      (if (null? item) json
                        (cons (make-json-pair item) json))
                      ) '() data)))
    (string-append "{" (string-join (reverse json) ", ") "}")))

(define (make-json-array data)
  (let ((json (fold (lambda (item json)
                      (if (null? item) json
                        (cons (make-json-hash item) json))
                      ) '() data)))
    (string-append "[" (string-join (reverse json) ", ") "]")))

(define (list->json data . callback)
  (let ((json (if (json-array? data)
                (make-json-array data)
                (make-json-hash data))))
    (if (not (null? callback))
      (string-append (car callback) "(" json ")")
      json)))


;(define (main args)
;  (let ((sample '((("@asap" "#f00") ("@neko" "#0f0")) (("@test" "#00f") ("@hello" "#0ff")))))
;    (print (list->json sample "neko"))
;    )
;  )

(provide "mi/json")
