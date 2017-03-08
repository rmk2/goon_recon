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
	   (esi-fill-alliance corporation)
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

(define (citadel-parse-scan input
			    #:corporation corporation
			    #:alliance alliance
			    #:location location
			    #:id [scan-id 0])
  (let ([citadel (hash-ref (dscan-proximity (citadel? input)) 'type)]
	[location-input (sql-parse->struct (parse-map location) #:struct mapDenormalize)])
    (call-with-values
	(lambda ()
	  (values
	   (mapDenormalize-region location-input)
	   (mapDenormalize-constellation location-input)
	   (mapDenormalize-system location-input)
	   (mapDenormalize-id location-input)
	   (esi-fill-alliance corporation)
	   (if (string-empty? corporation) "" (string-upcase corporation))
	   (srfi-date->sql-timestamp (current-date))
	   (parse-type :id citadel)
	   scan-id))
      sql-citadel)))

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

(define (pretty-print-citadel-result data result)
  (format "~a: ~a @ ~akm, belonging to ~a"
	  (parse-map :name (sql-citadel-location result))
	  (hash-ref (dscan-proximity (citadel? data)) 'type)
	  (hash-ref (dscan-proximity (citadel? data)) 'distance)
	  (if (sql-null? (sql-citadel-corporation result))
	      "-"
	      (string-append
	       (let ([corporation (parse-corporation (sql-citadel-corporation result))])
		 (if (false? corporation)
		     "? "
		     (vector-ref corporation 2)))
	       "["
	       (sql-citadel-corporation result)
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
	  (output:create-html-navigation #:audience (auth:try-authorization-header :subject req))
	  (div 'id: "content"
	       (h1 (pretty-print-location location-guess))
	       (cond [(and (false? corporation-try) (or (dscan-proximity (citadel? data)) (dscan-proximity (tower? data))))
		      (form 'method: "POST"
			    "Please enter a valid corporation ticker: "
			    (input 'type: "text"
				   'name: "corporation"
				   'maxlength: "5"
				   'size: "5"
				   'style: "margin-right:0.5em;"
				   'placeholder: "GEWNS"
				   'required: #t)
			    (map (lambda (name value)
				   (input 'type: "hidden" 'name: name 'value: (if (string-empty? value) "" value)))
				 (list "dscan" "alliance" "location")
				 (list dscan alliance corporation))
			    (input 'type: "submit" 'value: "Submit"))]
		     [(and (null? location-guess) (dscan-proximity (citadel? data)))
		      (form 'method: "POST"
			    "Please enter closest celestial: "
			    (input 'type: "text"
				   'name: "location"
				   'required: #t
				   'style: "margin-right:0.5em;"
				   'placeholder: "Jita IV - Moon 4"
				   'required: #t)
			    (map (lambda (name value)
				   (input 'type: "hidden" 'name: name 'value: (if (string-empty? value) "" value)))
				 (list "dscan" "alliance" "corporation")
				 (list dscan alliance corporation))
			    (input 'type: "submit" 'value: "Submit"))]
		     [else
		      (list
		       (b "Scan Result: ")
		       (cond
			[(not (false? citadel-scan-result)) (pretty-print-citadel-result data citadel-scan-result)]
			[(not (false? moon-scan-result)) (pretty-print-moon-result data moon-scan-result)]
			[(not (false? goo-scan-result))
			 (format "Moon probing results saved for ~a!"
				 (string-join (map caar (goo-split-probe-results (dscan-raw->list dscan)))
					      ","
					      #:before-last " & "
					      #:after-last ""))]
			[else "No structure found in close proximity"]))])
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

  (define location-try (parse-map location))

  (define corporation-try (esi-try-corporation corporation))

  (when (and (not (false? corporation-try))
	     (false? (corporation? corporation)))
    (sql-corporation-update-corporations
     (esi-hash-parse-corporation
      (esi-hash-poll-corporation (list corporation-try)))))

  (define moon-scan-result
    (cond
     [(or (string-empty? dscan)
	  (false? (dscan-proximity (tower? data)))
	  (false? (dscan-proximity (moon? data)))
	  (false? corporation-try)
	  (> (hash-ref (dscan-proximity (moon? data)) 'distance) (max_distance)))
      #f]
     [else (moon-parse-scan data #:corporation corporation #:alliance alliance #:id (dscan-local->string :id dscan))]))

  (define moon-empty-result
    (cond
     [(or (string-empty? dscan)
	  (dscan-proximity (tower? data))
	  (false? (dscan-proximity (moon? data)))
	  (> (hash-ref (dscan-proximity (moon? data)) 'distance) (max_distance)))
      #f]
     [else (moon-parse-empty data #:id (dscan-local->string :id dscan))]))

  (define goo-scan-result
    (cond
     [(or (string-empty? dscan)
	  (false? (goo-probe-result? dscan)))
      #f]
     [else (goo-list->struct (goo-parse-results (goo-split-probe-results (dscan-raw->list dscan))))]))

  (define citadel-scan-result
    (cond
     [(or (string-empty? dscan)
	  (false? (dscan-proximity (citadel? data)))
	  (> (hash-ref (dscan-proximity (citadel? data)) 'distance) (max_distance))
	  (and (null? location) (false? (dscan-celestials? data)))
	  (false? location-try)
	  (false? corporation-try))
      #f]
     [(and (dscan-proximity (tower? data))
	   (dscan-proximity (citadel? data))
	   (<= (hash-ref (dscan-proximity (citadel? data)) 'distance) (max_distance))
	   (<= (hash-ref (dscan-proximity (tower? data)) 'distance) (max_distance))
	   (> (hash-ref (dscan-proximity (citadel? data)) 'distance)
	      (hash-ref (dscan-proximity (tower? data)) 'distance)))
      #f]
     [else (citadel-parse-scan data
			       #:corporation corporation
			       #:alliance alliance
			       #:id (dscan-local->string :id dscan)
			       #:location (cond [(null? location) (dscan-nearest-celestial data)]
						[(not (false? location-try)) location]))]))

  (define location-guess
    (cond [(not (false? goo-scan-result))
	   (list (parse-region :name (sql-goo-region (first goo-scan-result)))
		 (parse-constellation :name (sql-goo-constellation (first goo-scan-result)))
		 (parse-solarsystem :name (sql-goo-system (first goo-scan-result))))]
	  [(and (not (false? citadel-scan-result))
		(string? location)
		(not (false? location-try)))
	   (let ([location-input (sql-parse->struct location-try #:struct mapDenormalize)])
	     (list (mapDenormalize-region location-input)
		   (mapDenormalize-constellation location-input)
		   (mapDenormalize-system location-input)))]
	  [else (guess->location (dscan-guess-location data))])) 

  (cond
   [(and persist-dscan?
	 (or (not (false? moon-scan-result))
	     (not (false? moon-empty-result))
	     (not (false? goo-scan-result))
	     (not (false? citadel-scan-result))))
    (begin
      (dscan-local->string :write dscan)
      (sql-scan-update-users
       (list (sql-scan (dscan-local->string :id dscan)
		       (auth:try-authorization-header :username req)
		       (cond [(not (false? moon-scan-result)) "moon"]
			     [(not (false? moon-empty-result)) "moon"]
			     [(not (false? goo-scan-result)) "goo"]
			     [(not (false? citadel-scan-result)) "citadel"]
			     [else "dscan"])
		       (srfi-date->sql-timestamp (current-date))))))])

  (cond
   [(not (false? citadel-scan-result))
    (sql-citadel-update-scan (list citadel-scan-result))]
   [(not (false? moon-scan-result))
    (sql-moon-update-scan (list moon-scan-result))]
   [(not (false? moon-empty-result))
    (sql-moon-update-empty (list moon-empty-result))]
   [(not (false? goo-scan-result))
    (sql-goo-update-scan goo-scan-result)])

  (send/back response-generator))
