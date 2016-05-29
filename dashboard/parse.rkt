#!/usr/bin/env racket
#lang racket

(require eve)
(require net/cgi)
(require racket/set)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

;; CGI bindings

(define cgi-data (get-bindings))
(define poll_dscan (extract-bindings 'dscan cgi-data))
(define poll_corporation (extract-bindings 'corporation cgi-data))
(define poll_alliance (extract-bindings 'alliance cgi-data))
(define poll_empty (extract-bindings 'empty cgi-data))

;; Parameters

(define max_distance (make-parameter 10000))

;; d-scan -> moon scan data

(define (moonscan [input data] #:corporation [corporation poll_corporation] #:alliance [alliance poll_alliance])
  (if (< (hash-ref (dscan-proximity (moon? input)) 'distance) (max_distance))
      (let ([moon (hash-ref (dscan-proximity (moon? input)) 'name)]
	    [tower (hash-ref (dscan-proximity (tower? input)) 'type)])
	(flatten
	 (list
	  (parse-map :region moon)
	  (parse-map :constellation moon)
	  (parse-map :system moon)
	  (cdr (split-moon-display moon))
	  (fill-alliance alliance corporation)
	  (if (string-empty? (car corporation)) sql-null (map string-upcase corporation))
	  (srfi-date->sql-timestamp (current-date))
	  (parse-type :id tower)
	  (if (and (dscan-proximity (forcefield? input))
		   (< (hash-ref (dscan-proximity (forcefield? input)) 'distance) (max_distance)))
	      1
	      0))))
      #f))

;; Pretty-print condensed d-scan result for HTML output

(define (pretty-print-dscan-result)
  (format "~a: ~a~a @ ~akm, belonging to ~a"
	  (hash-ref (dscan-proximity (moon? data)) 'name)
	  (parse-type :name (ninth moonscan-result))
	  (if (zero? (tenth moonscan-result))
	      " (offline) "
	      " (online) ")
	  (hash-ref (dscan-proximity (tower? data)) 'distance)
	  (if (sql-null? (seventh moonscan-result))
	      "-"
	      (string-append
	       (parse-corporation :name (seventh moonscan-result))
	       "["
	       (seventh moonscan-result)
	       "]"))))

;; Exec

(define data
  (dscan-list->hash
   (dscan-normalise-distance
    (dscan-raw->list (first poll_dscan)))))

(define moonscan-result
  (cond
   [(or (string-empty? (car poll_dscan))
	(false? (dscan-proximity (tower? data)))
	(false? (dscan-proximity (moon? data))))
    #f]
   [else (let ([result (moonscan data)])
	   (if (false? result)
	       #f
	       result))]))

(when (not (false? moonscan-result))
  (sql-moon-update-scan (list moonscan-result)))

(output-http-headers)
(output-xml (doctype 'html))
(output-xml
 (html
  (output:create-html-head #:title "Dashboard Scan Result" #:tablesorter #f)
  (body
   (div 'id: "content"
	(h1 (pretty-print-location (dscan-guess-location data)))
	(b "Scan Result: ")
	(if (false? moonscan-result)
	    "No tower found in close proximity"
	    (pretty-print-dscan-result))
	(br)
	(br)
	(hr)
	(form 'action: "./form.rkt"
	      (input 'type: "submit" 'value: "Return to Dashboard"))))))

;; TODO:

;; - Think hard about whether form.rkt needs to be a CGI script, or whether it
;; can be static HTML
