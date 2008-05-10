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
    twitter-public-timeline
    )
  )
(select-module twitter)

;; =twitter
;; Twitterクラス
;; ユーザー名、パスワード、リクエスト先のURIなどを扱う。
;; ---------------------------------------------------------
(define-class <twitter> ()
  ((user :init-keyword :user :init-value "")
   (password :init-keyword :password :init-value "")
   (formats :init-keyword :formats :init-value "xml")
   (base-uri :init-value "http://twitter.com/")
   (public-timeline :init-value "statuses/public_timeline.")
   (friend-timeline :init-value "statuses/friends_timeline.")
   (user-timeline :init-value "statuses/user_timeline.")
   ;(show :init-value "statuses/show/[[id]].")
   (update :init-value "statuses/update.")
   ;(replies :init-value "")
   ;(destroy :init-value "")
   ;(friends :init-value "")
   ;(followers :init-value "")
   ;(featured :init-value "")
   ;(user-show :init-value "")
   ;(direct-messages :init-value "")
   ;(direct-message-sent :init-value "")
   (status-max :init-value 160)
   (is-xml :init-value #t)
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
    (set! (ref self 'is-xml) (if (string=? formats "xml") #t #f))
    (for-each
      (lambda (tag)
        (set! (ref self tag) #`",base,(ref self tag),formats")
        ) '(public-timeline update friend-timeline user-timeline))
    )
  )

;; privates
(define (my-conv str) (ces-convert (uri-encode-string str) "*JP"))
(define (to-sxml str) (ssax:xml->sxml (open-input-string str) '()))

;; =twitter?
;; @objがTwitterオブジェクトかどうかを返す
;; ---------------------------------------------------------
(define (twitter? obj)
  (eq? (class-of obj) <twitter>)
  )

;; =twitter-public-timeline
;; パブリックタイムラインを返す
;; @argがTwitterオブジェクトならそのフォーマットで結果を返し、
;; ちがったら@argsをフォーマットとして結果を返す
;; ---------------------------------------------------------
(define (twitter-public-timeline arg)
  (let1 obj (if (twitter? arg) arg (make <twitter> :formats arg))
    (let1 res (open-uri (ref obj 'public-timeline))
      (if (ref obj 'is-xml) (to-sxml res) res)
      )
    )
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
      (let1 res (open-uri uri :method 'post :user/pass (ref twitter-obj 'user/pass))
        (if (ref twitter-obj 'is-xml) (to-sxml res) res)
        )
      )
    )
  )

;; =twitter-friend-timeline
;; @twitter-objに格納されている情報から
;; フレンドのタイムラインを取得し、SXMLとして結果を返す
;; ---------------------------------------------------------
(define (twitter-friend-timeline twitter-obj)
  (let1 res (open-uri (ref twitter-obj 'friend-timeline)
                      :user/pass (ref twitter-obj 'user/pass))
    (if (ref twitter-obj 'is-xml) (to-sxml res) res)
    )
  )

;; =twitter-user-timeline
;; @twitter-objに格納されている情報から
;; 自分のタイムラインを取得し、SXMLとしｔ結果を返す
;; ---------------------------------------------------------
(define (twitter-user-timeline twitter-obj)
  (let1 res (open-uri (ref twitter-obj 'user-timeline)
                      :user/pass (ref twitter-obj 'user/pass))
    (if (ref twitter-obj 'is-xml) (to-sxml res) res)
    )
  )

(provide "twitter")
