#lang racket

(require eve)

(require net/uri-codec)

(require "common.rkt")

(provide (all-defined-out))

;; d-scan -> scan data (using the sql-moon struct)

(define (moon-parse-scan input #:corporation corporation #:alliance alliance)
  (let ([moon (hash-ref (dscan-proximity (moon? input)) 'name)]
	[tower (hash-ref (dscan-proximity (tower? input)) 'type)])
    (call-with-values
	(lambda ()
	  (values
	   (parse-map :region moon)
	   (parse-map :constellation moon)
	   (parse-map :system moon)
	   (first (cdr (split-moon-display moon)))
	   (second (cdr (split-moon-display moon)))
	   (fill-alliance #:alliance alliance #:corporation corporation)
	   (if (string-empty? corporation) "" (string-upcase corporation))
	   (srfi-date->sql-timestamp (current-date))
	   (parse-tower :id tower)
	   (if (and (dscan-proximity (forcefield? input))
		    (< (hash-ref (dscan-proximity (forcefield? input)) 'distance) (max_distance)))
	       1
	       0)))
      sql-moon)))

(define (moon-parse-empty input)
  (let ([moon (hash-ref (dscan-proximity (moon? input)) 'name)])
    (call-with-values
	(lambda ()
	  (values
	   (parse-map :region moon)
	   (parse-map :constellation moon)
	   (parse-map :system moon)
	   (first (cdr (split-moon-display moon)))
	   (second (cdr (split-moon-display moon)))
	   null
	   null
	   (srfi-date->sql-timestamp (current-date))
	   null
	   null))
      sql-moon)))

;; Pretty-print condensed d-scan result for HTML output

(define (pretty-print-moon-result data result)
  (format "~a: ~a~a @ ~akm, belonging to ~a"
	  (hash-ref (dscan-proximity (moon? data)) 'name)
	  (hash-ref (dscan-proximity (tower? data)) 'type)
	  (if (zero? (sql-moon-online result))
	      " (offline) "
	      " (online) ")
	  (hash-ref (dscan-proximity (tower? data)) 'distance)
	  (if (sql-null? (sql-moon-corporation result))
	      "-"
	      (string-append
	       (let ([corporation (parse-corporation (sql-moon-corporation result))])
		 (if (false? corporation)
		     "? "
		     (vector-ref corporation 2)))
	       "["
	       (sql-moon-corporation result)
	       "]"))))

;; servlet

(define (exec-result req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head #:title "Dashboard Scan Result" #:tablesorter #f #:navigation #t)
	 (body
	  (output:create-html-navigation #:title "GoonSwarm Recon")
	  (div 'id: "content"
	       (h1 (pretty-print-location (guess->location (dscan-guess-location data))))
	       (b "Scan Result: ")
	       (cond
		[(not (false? moon-scan-result)) (pretty-print-moon-result data moon-scan-result)]
		[else "No structure found in close proximity"])
	       (br)
	       (br)
	       (hr)
	       (form 'action: "report" (input 'type: "submit" 'value: "Return to Dashboard")))))
	port))))
  
  (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
  (define form-data (form-urlencoded->alist post-data))

  (define-values (corporation alliance dscan)
    (vector->values
     (list->vector
      (map cdr (form-urlencoded->alist post-data)))))
  
  (define data
    (dscan-sort
     (dscan-list->hash
      (dscan-normalise-distance
       (dscan-raw->list dscan)))))

  (define moon-scan-result
    (cond
     [(or (string-empty? dscan)
	  (false? (dscan-proximity (tower? data)))
	  (false? (dscan-proximity (moon? data)))
	  (> (hash-ref (dscan-proximity (moon? data)) 'distance) (max_distance)))
      #f]
     [else (moon-parse-scan data #:corporation corporation #:alliance alliance)]))

  (define moon-empty-result
    (cond
     [(or (string-empty? dscan)
	  (dscan-proximity (tower? data))
	  (false? (dscan-proximity (moon? data)))
	  (> (hash-ref (dscan-proximity (moon? data)) 'distance) (max_distance)))
      #f]
     [else (moon-parse-empty data)]))

  (cond
   [(not (false? moon-scan-result))
    (sql-moon-update-scan (list moon-scan-result))]
   [(not (false? moon-empty-result))
    (sql-moon-update-empty (list moon-empty-result))])

  (send/back response-generator))
