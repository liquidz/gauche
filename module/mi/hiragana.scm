(define-module mi.hiragana (export number->hiragana))
(select-module mi.hiragana)

; 桁の宣言
(define digit-class '("じゅう" ("ひゃく" "びゃく") ("せん" "ぜん") "まん"))
; 特殊な桁の読み方をする数字（３）の宣言
(define special-digit #\3)
; 数字とひらがなの対応表
(define digits
  `(
    (#\1 "いち") (#\2 "に") (,special-digit "さん") (#\4 "よん")
    (#\5 "ご") (#\6 "ろく") (#\7 "なな") (#\8 "はち") (#\9 "きゅう")
    ))
; 最初の数字は桁を付けないため、対応するひらがなを宣言しておく
(define first-digit (cadar digits))
; 特殊な桁の読み方をする数字に対応するひらながの宣言
(define special-hiragana (cadr (find (lambda (x) (char=? special-digit (car x))) digits)))

; =get-digit-class
; @indexから桁に対応するひらがなを返す。
; なお@num-hiraganaが特殊な桁の読み方をする場合は、特殊な読み方を返す
; ---------------------------------------------------------------------------
(define (get-digit-class num-hiragana index)
  (let1 dclass (list-ref digit-class index)
    (if (pair? dclass)
      (if (string=? num-hiragana special-hiragana) (cadr dclass) (car dclass))
      dclass
      )
    )
  )

; =number-char->hiragana
; １文字の@num-charに対応するひらがなでの読み方を返す
; ---------------------------------------------------------------------------
(define (number-char->hiragana num-char)
  (cadr (find (lambda (x)
                (char=? num-char (car x))
                ) digits)
        )
  )

; =number->hiragana
; @numに対応するひらがなを作り返す
; ---------------------------------------------------------------------------
(define (number->hiragana num)
  (let1 hiragana-list (reverse (map number-char->hiragana (string->list (number->string num))))
    (let loop((ls (cdr hiragana-list)) (index 0) (result (car hiragana-list)))
      (if (null? ls)
        result
        (loop (cdr ls) (+ 1 index)
              (string-append
                (if (string=? (car ls) first-digit) "" (car ls))
                (get-digit-class (car ls) index) result))
        )
      )
    )
  )

(provide "mi/hiragana")

