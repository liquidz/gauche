(use srfi-1)
(use srfi-13)

(define (３の倍数 x) (= 0 (modulo x 3)))
(define (３のつく数字 x) (string-scan (number->string x) "3"))
(define (アホ x) (string-append x "～っ")) 
(define (８の倍数 x) (= 0 (modulo x 8)))
(define (気持ちよく x) (string-append x "ぁん"))


(define (世界のナベアツ start _ end __ . args)
  (define (or-all target fns)
    (fold (lambda (fn res)
            (or res (fn target))
            ) #f fns))
  (define (make-conditions data)
    (fold (lambda (con res)
            (if (list? con)
              (cons (fold (lambda (x ls)
                            (if (keyword? x) ls (cons x ls))
                            ) '() con)
                    res)
              res)
            ) '() data)
    )

  (let1 conditions (make-conditions args)
    (map (lambda (i)
           (fold (lambda (co res)
                   (if (or-all i (cdr co)) ((car co) res) res)
                   ) (number->string i) conditions)
           ) (iota end start))
    )
  )

(define (main args)
  (print
    (世界のナベアツ 1 :から 40 :まで数えて
                    `(,３の倍数 :と ,３のつく数字 :の時だけ ,アホ) :になって
                    `(,８の倍数 :の時だけ ,気持ちよく) :なります。)
    )
  )