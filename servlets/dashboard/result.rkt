#lang racket

(require eve)

(require "common.rkt")

(provide (all-defined-out))

;; d-scan -> scan data (using the sql-moon struct)

(define (moon-parse-scan input #:corporation corporation #:alliance alliance #:id [scan-id 0])
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
	       0)
	   scan-id))
      sql-moon)))

(define (moon-parse-empty input #:id [scan-id 0])
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
	   null
	   scan-id))
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

(define (exec-result req #:persist-dscan persist-dscan?)
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
	       (let ([location (cond [(not (false? goo-scan-result))
				      (list (parse-region :name (sql-goo-region (first goo-scan-result)))
					    (parse-constellation :name (sql-goo-constellation (first goo-scan-result)))
					    (parse-solarsystem :name (sql-goo-system (first goo-scan-result))))]
				     [else (guess->location (dscan-guess-location data))])])
		 (h1 (pretty-print-location location)))
	       (b "Scan Result: ")
	       (cond
		[(not (false? moon-scan-result)) (pretty-print-moon-result data moon-scan-result)]
		[(not (false? goo-scan-result))
		 (format "Moon probing results saved for ~a!"
			 (string-join (map caar (goo-split-probe-results (dscan-raw->list dscan)))
				      ","
				      #:before-last " & "
				      #:after-last ""))]
		[else "No structure found in close proximity"])
	       (br)
	       (br)
	       (hr)
	       (form 'target: "_self" (input 'type: "submit" 'value: "Return to Dashboard")))))
	port))))
  
  (define-values (corporation alliance dscan location)
    (values
     (extract-post-data req #"corporation")
     (extract-post-data req #"alliance")
     (extract-post-data req #"dscan")
     (extract-post-data req #"location")))
  
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
     [else (moon-parse-scan data #:corporation corporation #:alliance alliance #:id (dscan-data->id dscan))]))

  (define moon-empty-result
    (cond
     [(or (string-empty? dscan)
	  (dscan-proximity (tower? data))
	  (false? (dscan-proximity (moon? data)))
	  (> (hash-ref (dscan-proximity (moon? data)) 'distance) (max_distance)))
      #f]
     [else (moon-parse-empty data #:id (dscan-data->id dscan))]))

  (define goo-scan-result
    (cond
     [(or (string-empty? dscan)
	  (false? (goo-probe-result? dscan)))
      #f]
     [else (goo-list->struct (goo-parse-results (goo-split-probe-results (dscan-raw->list dscan))))]))

  (cond
   [(and persist-dscan?
	 (or (not (false? moon-scan-result))
	     (not (false? moon-empty-result))
	     (not (false? goo-scan-result))))
    (dscan-gzip-write dscan)])

  (cond
   [(not (false? moon-scan-result))
    (sql-moon-update-scan (list moon-scan-result))]
   [(not (false? moon-empty-result))
    (sql-moon-update-empty (list moon-empty-result))]
   [(not (false? goo-scan-result))
    (sql-goo-update-scan goo-scan-result)])

  (send/back response-generator))
