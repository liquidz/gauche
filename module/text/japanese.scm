(define-module text.japanese
  (export-all)
  )
(select-module text.japanese)

(define jp-reg #/[一-龠]+|[ァ-ヴー]+|[a-zA-Z0-9]+|[ａ-ｚＡ-Ｚ０-９]+/)

(define (get-simple-japanese-words original-string)
  (let loop((str original-string) (result '()))
    (let1 m (jp-reg str)
      (if m
        (loop (m 'after) (cons (m) result))
        result
        )
      )
    )
  )


(provide "text/japanese")

