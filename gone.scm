#!/usr/local/bin/gosh

(use gauche.parseopt)

(define (main args)
  (parse-options
	(cdr args)
	(("e=es" (code file)
	  (with-input-from-file
		file
		(lambda ()
		  (port-fold (lambda (line count)
					   (eval `(let ((_ ,line) (i ,count))
								,code) (interaction-environment))
					   (+ count 1)
					   ) 1 read-line)
		  ))
	  )
	 )
	)
  )

