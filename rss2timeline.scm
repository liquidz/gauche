(use dbi)
(use gauche.collection)
(use gauche.sequence)
(use gauche.charconv)
(use rfc.uri)
(use rfc.http)
(use sxml.ssax)
(use sxml.sxpath)

(use nifty.timeline)

(define (rss:get item key)
  (let1 ls ((sxpath `(,key)) item) 
    (if (> (length (car ls)) 1) (cadar ls) "")
    )
  )

(define (parse-rss-item item)
  (let ((title (rss:get item 'title))
        (link (rss:get item 'link))
        (description (rss:get item 'description))
        (date (rss:get item 'pubDate))
        (author (rss:get item 'author)))
    (values (if (> (string-length title) 50) (substring title 0 49) title)
            link description date author)
    )
  )

(define day-count (make-hash-table))

(define (get-day-count day)
  (let1 day-symbol (string->symbol (car (string-split day " ")))
    (let1 result (hash-table-get day-count day-symbol 0)
      (hash-table-put! day-count day-symbol (+ 1 result))
      result
      )
    )
  )

(define (main args)
  (receive (status header body) (http-get "172.19.45.30:4000" "/rdf")
    (let1 sxml (ssax:xml->sxml (open-input-string body) '())

      (for-each
        (lambda (item)
          (receive (title link description date author) (parse-rss-item item)
            (let1 grade (get-day-count date)
              (print "adding " title "....")
              (receive (result message)
                (post-to-timeline :timeline-key "d4f63c78f27acb3cfdd9f225771a2e61"
                                  :timeline-id "7159"
                                  :title title
                                  :description (string-append author "\n\n" description)
                                  :start-time date
                                  :end-time date
                                  :link link
                                  :grade (* grade 5)
                                  )
                (if result
                  (sys-sleep 1)
                  (print (ces-convert message "*jp"))
                  )
                )
              )
            )
          )
        ((sxpath '(rss channel item)) sxml))
      )
    )
  )

