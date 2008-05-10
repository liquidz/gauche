;;; Twitter Module for Gauche
;;; @author liquidz

(define-module twitter
  (use rfc.uri)
  (use gauche.charconv)
  (use sxml.ssax)
  (use mi.http)
  (export
    with-twitter twitter-update
    twitter-friend-timeline twitter-user-timeline
    )
  )
(select-module twitter)

;; =twitter
;; Twitterクラス
;; ユーザー名、パスワード、リクエスト先のURIなどを扱う。
;; ---------------------------------------------------------
(define-class <twitter> ()
  ((user :init-keyword :user)
   (password :init-keyword :password)
   (formats :init-keyword :formats :init-value "xml")
   (base-uri :init-value "http://twitter.com/")
   (update :init-value "statuses/update.")
   (friend-timeline :init-value "statuses/friends_timeline.")
   (user-timeline :init-value "statuses/user_timeline.")
   (status-max :init-value 160)
   (user/pass)
   )
  )

;; =initialize
;; Twitterクラスの初期化メソッド
;; 取得したいフォーマットからリクエスト先URIなどを生成する
;; ---------------------------------------------------------
(define-method initialize ((self <twitter>) initargs)
  (next-method)
  (let ((user (ref self 'user)) (password (ref self 'password))
        (base (ref self 'base-uri)) (formats (ref self 'formats)))
    (set! (ref self 'user/pass) #`",|user|:,password")
    (for-each
      (lambda (tag)
        (set! (ref self tag) #`",base,(ref self tag),formats")
        ) '(update friend-timeline user-timeline))
    )
  )

;; =my-conv
;; @strをURIエンコード＆UTF-8変換して返す
;; ---------------------------------------------------------
(define (my-conv str)
  (ces-convert (uri-encode-string str) "*JP")
  )
;; =to-sxml
;; @strをSXMLに変換して返す
;; ---------------------------------------------------------
(define (to-sxml str)
  (ssax:xml->sxml (open-input-string str) '())
  )

;; =with-twitter
;; @argsからTwitterオブジェクトを生成し、@fnに処理を渡す
;; ---------------------------------------------------------
(define (with-twitter fn . args)
  (let-keywords args ((user "") (password ""))
    (unless (or (string=? user "") (string=? password ""))
      (fn (make <twitter> :user user :password password))
      )
    )
  )

;; =twitter-update
;; @twitter-objに格納されている情報から
;; @statusをステータスとして更新する
;; ---------------------------------------------------------
(define (twitter-update twitter-obj status)
  (if (< (string-length status) (ref twitter-obj 'status-max))
    (let1 uri #`",(ref twitter-obj 'update)?status=,(my-conv status)"
      (to-sxml
        (open-uri uri :method 'post :user/pass (ref twitter-obj 'user/pass))
        )
      )
    )
  )

;; =twitter-friend-timeline
;; @twitter-objに格納されている情報から
;; フレンドのタイムラインを取得し、SXMLとして結果を返す
;; ---------------------------------------------------------
(define (twitter-friend-timeline twitter-obj)
  (to-sxml
    (open-uri (ref twitter-obj 'friend-timeline)
              :user/pass (ref twitter-obj 'user/pass))
    )
  )

;; =twitter-user-timeline
;; @twitter-objに格納されている情報から
;; 自分のタイムラインを取得し、SXMLとしｔ結果を返す
;; ---------------------------------------------------------
(define (twitter-user-timeline twitter-obj)
  (to-sxml
    (open-uri (ref twitter-obj 'user-timeline)
              :user/pass (ref twitter-obj 'user/pass))
    )
  )

(provide "twitter")
