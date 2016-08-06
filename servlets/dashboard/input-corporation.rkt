#lang racket

(require eve)

(provide (all-defined-out))

(define (exec-input-corporation req param)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head #:title "Unknown Corporation Input" #:tablesorter #f #:navigation #f)
	 (body
	  (div 'id: "content")
	  (h1 "Unknown Corporation Input")
	  (cond
	   [(not (false? corporation-result))
	    (map (lambda (corp) (p (string-join corp " » "))) corporation-result)]
	   [else
	    (p (string-append "Invalid corporationIDs: " (string-join input ", " #:before-last " & ")))])))
	port))))

  (define input
    (cond
     [(string? param)
      (remove-duplicates (filter (lambda (n) (regexp-match? #px"[0-9]+" n)) (string-split param ",")))]
     [(number? param) (list (number->string param))]))

  (define corporations
    (exec-limit-api-rate #:function hash-poll-corporations
			 #:input input
			 #:delay 1
			 #:limit 30
			 #:digest (lambda (in)
				    (hash-parse-corporations
				     (filter-map (lambda (hash)
						   (if (hash-has-key? hash 'corporationID)
						       hash
						       #f))
						 in)))))

  (define corporation-result
    (cond
     [(empty? corporations) #f]
     [else corporations]))

  (cond [(not (false? corporation-result))
	 (sql-corporation-update-corporations corporation-result)])

  (send/back response-generator))
