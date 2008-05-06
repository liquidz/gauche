(use mi.generic)
(use mi.abbrev)
(use mi.http)
(use gauche.charconv)
(use sxml.serializer)

; -----
; todo
; * search iframe automatically
; * referer auto complete => wget --referer ***

(define babypink
  '(
    :site     "美少女☆ベイビーピンク"
    :uri      "http://www.kk.iij4u.or.jp/~babypink/iframe/sample_movie.htm"
    :title    #/タイトル<\/span><span class=\"text12line\">\/ (.+?)<br>/
    :link     #/(http:\/\/www\.babypink\.to\/movie\/[0-9]+\.htm)/
    :image    #/(http:\/\/www\.babypink\.to\/movie\/img_mov\/[0-9]+\/[0-9]+\.jpg)/
    :priority (image title link)
    :download #/(http:.+?\.zip)/
    )
  )

(define cpz
  '(
    :site     "CPZオンライン"
    :uri      "http://cpz.to/main.html"
    :title    #/<h3><a .+?>(.+?)<\/a><\/h3>/
    :link     #/(http:\/\/cpz\.to\/movie\-list\/[0-9]+\.htm)/
    :image    #/(http:\/\/mxserver15\.net\/cpz\/movie\/[0-9]+\/[0-9]+\.jpg)/
    :priority (image title link)
    :download #/(http:.+?w\.zip)/
    )
  )

(define movie-head-loop
  '(
    :site     "MOVIE HEAD LOOP"
    :uri      "http://www.headloop.com/main.html"
    :title    #/<h3>タイトル ： (.+?)<\/h3>/
    :link     #/(http:\/\/www\.headloop\.com\/movie\/[0-9]+\.html)/
    :image    #/(http:\/\/img\.jamhendrix\.com\/head\/movie\/.+?\/[0-9]+\.jpg)/
    :priority (image title link)
    :download #/(http:.+?\.zip)/
    )
  )

(define (print-each ls)
  (for-each (lambda (x) (print x)) ls)
  )

; =get-with-priority
; 優先度の順にデータを取得する
(define (get-with-priority contents title-reg link-reg image-reg priority-list)
  (let loop((title "") (link "") (image "") (priority priority-list) (html contents))
    (if (null? priority)
      (values title link image html)
      (let1 pri (car priority)
        (cond
          [(eq? 'title pri)
           (let1 res (title-reg html)
             (if res
               (loop (ces-convert (res 1) "*JP") link image (cdr priority) (res 'after))
               (values title link image html)
               )
             )
           ]
          [(eq? 'link pri)
           (let1 res (link-reg html)
             (if res
               (loop title (res 1) image (cdr priority) (res 'after))
               (values title link image html)
               )
             )
           ]
          [(eq? 'image pri)
           (let1 res (image-reg html)
             (if res
               (loop title link (res 1) (cdr priority) (res 'after))
               (values title link image html)
               )
             )
           ]
          [else
            (values title link image html)
            ]
          )
        )
      )
    )
  )

; =get-download-links
; @uriから正規表現@download-regを元にアーカイブのリンクを抽出しリストとして返す
(define (get-download-links uri download-reg)
  (receive (body server req) (open-uri uri)
    (uniq (reverse (fold (lambda (x res)
                           (let1 matched (download-reg x)
                             (if matched (cons (matched 1) res) res)
                             )
                           ) '() (string-split body "href"))
                   )
          )
    )
  )

(define (get-movie-links target)
  (let-keywords target ((site "") (uri "") (title #//) (link #//)
                                  (image #//) (priority '()) (download ""))
    (receive (body server req) (open-uri uri)
      (let loop((html body) (result '()))
        (receive (title-part link-part image-part html-rest)
          (get-with-priority html title link image priority)
          (cond
            [(or (string=? title-part "") (string=? link-part "") (string=? image-part ""))
             result
             ]
            [else
              (let1 download-links (get-download-links link-part download)
                (loop html-rest
                      (append result (list (list site title-part link-part download-links image-part))))
                )
              ]
            )
          )
        )
      )
    )
  )

(define (to-xml ls)
  (srl:sxml->xml
    `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
            ,(fold (lambda (x res)
                     (append res
                             (list (list 'item
                                         (list 'site (ref x 0))
                                         (list 'title (ref x 1))
                                         (list 'link (ref x 2))
                                         (fold (lambda (src links)
                                                 (append links `((src ,src)))
                                                 ) '(download) (ref x 3))
                                         (list 'image (ref x 4))
                                         )
                                   ))) '(movie-list) ls)
            )
    )
  )


(define (main args)
  (print
    (to-xml
      (append
        (get-movie-links cpz)
        (get-movie-links babypink)
;        (get-movie-links movie-head-loop)
        )
      )
    )
  )
