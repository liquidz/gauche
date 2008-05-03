(define-module mudalgorithm.muddy-hiragana
  (use srfi-1)
  (export number->hiragana))
(select-module mudalgorithm.muddy-hiragana)

; 桁の宣言
(define digit-class '("" "まん" "おく" "ちょう" "けい"))

; ひらがなへの対応表
(define digits
  '(("" "いち" "に" "さん" "よん" "ご" "ろく" "なな" "はち" "きゅう")
    ("" "じゅう" "にじゅう" "さんじゅう" "よんじゅう" "ごじゅう"
     "ろくじゅう" "ななじゅう" "はちじゅう" "きゅうじゅう")
    ("" "ひゃく" "にひゃく" "さんびゃく" "よんひゃく" "ごひゃく"
     "ろっぴゃく" "ななひゃく" "はっぴゃく" "きゅうひゃく")
    ("" "せん" "にせん" "さんぜん" "よんせん" "ごせん"
     "ろくせん" "ななせん" "はっせん" "きゅうせん")
    )
  )

; =get-hiragana
; 数字と単位から数字に対応するひらがなを取得
; -------------------------------------------------------------
(define (get-hiragana char index)
  (ref (ref digits index) (- (char->integer char) 48))
  )

; =three-class-number->hiragana
; 千の位までの数をひらがなに変換する
; -------------------------------------------------------------
(define (three-class-number->hiragana num)
  (let loop((ls (reverse (string->list (number->string num)))) (index 0) (res ""))
    (if (null? ls) res
      (loop (cdr ls) (+ index 1)
            (string-append (get-hiragana (car ls) index) res))
      )
    )
  )

; =number->hiragana
; 数に対応するひらがなを取得し返す
; -------------------------------------------------------------
(define (number->hiragana num)
  ; 千の位ごとに取得するための定数
  (define split-num 10000)

  (let loop((i num) (class 0) (res ""))
    ; 千の位ごとにひらがなに変換し、桁の読み方をくっつける
    (let ((amari (modulo i split-num)) (shou (floor (/ i split-num))))
      (if (and (= amari 0) (= shou 0)) res
        (loop shou (+ class 1)
              (string-append (three-class-number->hiragana amari)
                             (ref digit-class class) res)
              )
        )
      )
    )
  )

(provide "mudalgorithm/muddy-hiragana")
