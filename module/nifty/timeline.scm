(define-module nifty.timeline
  (use gauche.charconv)
  (use rfc.http)
  (use rfc.uri)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use srfi-19)
  (export with-timeline post-to-timeline timeline-duplicated?)
  )
(select-module nifty.timeline)

; =uri-utf8-encode
; private utility method to convert string
; -----------------------------------------------------------------------
(define (uri-utf8-encode str)
  (let1 ces (ces-guess-from-string str "*jp")
    (if (eq? ces 'utf-8) str
      (uri-encode-string (ces-convert str "*jp"))
      )
    )
  )

; =today
; private utility method to get today's date string
; -----------------------------------------------------------------------
(define (today)
  (let1 now (current-date)
    (let ((year (date-year now)) (month (date-month now)) (day (date-day now)))
      #`",|year|-,|month|-,|day|"
      )
    )
  )

; =with-timeline
; public method to use this module
; -----------------------------------------------------------------------
(define (with-timeline fn . args)
  (let-keywords args ((timeline-key "") (timeline-id ""))
    (let1 timeline-obj (make <timeline> :timeline-key timeline-key :timeline-id timeline-id)
      (fn timeline-obj)
      )
    )
  )

; =timeline
; basic class for this module
; -----------------------------------------------------------------------
(define-class <timeline> ()
  ((timeline-key :init-keyword :timeline-key :init-value "" :getter get-timeline-key)
   (timeline-id :init-keyword :timeline-id :init-value "" :getter get-timeline-id)
   (*server* :init-value "api.timeline.nifty.com" :allocation :class :getter get-server)
   (*post-request* :init-value "/api/v1/articles/create" :allocation :class :getter get-post-request)
   (*search-request* :init-value "/api/v1/articles/search" :allocation :class :getter get-search-request)
   )
  )

; =make-post-data
; private utility method to make post data string
; -----------------------------------------------------------------------
(define-method make-post-data ((obj <timeline>) args)
  (let-keywords args ((title "tmp") (description "tmp") (start-time (today))
                                    (end-time (today)) (grade 0) (link ""))
    (if (or (string=? (get-timeline-key obj) "") (string=? (get-timeline-id obj) "")) ""
      (let ((key (get-timeline-key obj))
            (id (get-timeline-id obj))
            (u-title (uri-utf8-encode title ))
            (u-description (uri-utf8-encode description))
            (u-link (uri-encode-string link)))
        (string-append #`"timeline_key=,|key|&timeline_id=,|id|&title=,|u-title|&description=,|u-description|&start_time=,|start-time|&end_time=,|end-time|&grade=,|grade|" (if (string=? u-link "") "" #`"&link=,|u-link|"))
        )
      )
    )
  )

; =make-search-data
; private utility method to make search data string
; -----------------------------------------------------------------------
(define-method make-search-data ((obj <timeline>) date hits)
  (let ((key (get-timeline-key obj)) (id (get-timeline-id obj)))
    #`"timeline_key=,|key|&timeline_id=,|id|&start_time=,|date|&end_time=,|date|&hits=,|hits|"
    )
  )


; =post-to-timeline
; public method to post article to timeline
; -----------------------------------------------------------------------
(define-method post-to-timeline ((obj <timeline>) . args)
  (let ((post-data (make-post-data obj args))
        (server (get-server obj))
        (post-request (get-post-request obj)))
    (if (string=? post-data "")
      (values #f "invalid parameters")
      (receive (status header body) (http-post server post-request post-data)
        (let1 sxml (ssax:xml->sxml (open-input-string body) '())
          (if (string=? (cadar ((sxpath '(response status code)) sxml)) "200")
            (values #t "ok")
            (values #f (cadar ((sxpath '(response status message)) sxml)))
            )
          )
        )
      )
    )
  )

; hash to cache search result
(define search-result-cache (make-hash-table))

; =get-search-result
; return search result. if cache exists, return cached date
; -----------------------------------------------------------------------
(define-method get-search-result ((obj <timeline>) date hits)
  (let1 s-date (string->symbol date)
    (if (hash-table-exists? search-result-cache s-date)
      (hash-table-get search-result-cache s-date)
      (let ((search-data (make-search-data obj date hits)) (server (get-server obj)) (search-request (get-search-request obj)))
        (receive (status header body) (http-post server search-request search-data)
          (let1 sxml (ssax:xml->sxml (open-input-string body) '())
            (hash-table-put! search-result-cache s-date sxml)
            sxml
            )
          )
        )
      )
    )
  )

; =timeline-duplicated?
; public method to check a timeline article duplicated or not
; -----------------------------------------------------------------------
(define-method timeline-duplicated? ((obj <timeline>) . args)
  (let-keywords args ((title "") (date "") (hits "1000"))
    (let1 sxml (get-search-result obj date hits)
      (call/cc (lambda (cc)
                 (fold (lambda (article result)
                         (let1 article-title ((sxpath '(title)) article)
                           (if (and (> (length (car article-title)) 1) (string=? (cadar article-title) title))
                             (cc #t) #f
                             )
                           )
                         ) #f ((sxpath '(response result articles article)) sxml))
                 ))
      )
    )
  )

(provide "nifty/timeline")

