;(use mudalgorithm.hiragana)
(use srfi-1)
(use sxml.serializer)
(use sxml.sxpath)
(use sxml.tools)

(use gauche.interactive)

#|
(define-class <svg> ()
  ((sxml :init-value '())
   (width :init-keyword :width :init-value 100)
   (height :init-keyword :height :init-value 100)
   (version :init-keyword :version :init-value "1.0")
   (encoding :init-keyword :encoding :init-value "UTF-8")
   (svg-base)

   (base :init-value
         '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
                 (svg (@ (xmlns:svg "http://www.w3.org/2000/svg")
                         (xmlns "http://www.w3.org/2000/svg")
                         (xmlns:xlink "http://www.w3.org/1999/xlink")
                         (version "1.0") (xml:space "defualt")
                         (width "100") (height "100"))
                      )
                 )
         )
   )
  )
(define-method initialize ((self <svg>) initargs)
  (next-method)
  (let ((width (ref self 'width))
        (height (ref self 'height))
        (version (ref self 'version))
        (encoding (ref self 'encoding)))
    (set! (ref self 'svg-base) `(*TOP* (*PI* xml ,#`"version=\",|version|\" encoding=\",|encoding|\"")))
    (set! (ref self 'sxml)
      (append
        (ref self 'svg-base)
        `((svg (@ (xmlns:svg "http://www.w3.org/2000/svg")
                  (xmlns "http://www.w3.org/2000/svg")
                  (xmlns:xlink "http://www.w3.org/1999/xlink")
                  (version "1.0") (xml:space "defualt")
                  (width ,width) (height ,height))
               )
          ))
      )
    )
  )

(define svg:root '(svg))

(define-method get-svg ((obj <svg>))
  (srl:sxml->xml (ref obj 'sxml))
  )

(define-method svg:add-element! ((obj <svg>) element)
  (set! (ref obj 'sxml) (svg:add-element obj element))
  )

(define-method svg:add-element ((obj <svg>) element)
  (let1 sxml (ref obj 'sxml)
    (append (ref obj 'svg-base)
            (list (sxml:change-content
                    (ref ((sxpath svg:root) sxml) 0)
                    (list element)
                    )))
    )
  )
|#

(define (with-svg-canvas fn . args)
  (let-keywords args ((width 100) (height 100))
    (fn (make <svg> :width width :height height))
    )
  )

(define-syntax keyword->symbol
  (syntax-rules ()
    [(_ key)
     (if (keyword? key) (string->symbol (keyword->string key)) key)
     ]
    )
  )

(define (svg tag . args)
  (let loop((ls args) (attributes '()) (value '()))
    (if (null? ls)
      (append `(,tag ,(append '(@) attributes)) (if (null? value) '() value))
      (if (keyword? (car ls))
        (loop (cddr ls) (cons (list (keyword->symbol (car ls)) (cadr ls)) attributes) value)
        (loop (cdr ls) attributes (cons (car ls) value))
        )
      )
    )
  )

(define (main args)
  (print (svg 'text :x 10 :y 20))
  ;(print (svg: text :x 10 :y 20 :width 50 :height 30 "hello"))
  #;(with-svg-canvas
    (lambda (c)
      (print "\nadd-element = " (svg:add-element c '(rect hello)) "\n")

      (print "original = " (ref c 'sxml))
      ;(svg:rect! c :width 50 :height 50)
      ;(print (get-svg c))
      )
    :width 500
    :height 200
    )
  )
