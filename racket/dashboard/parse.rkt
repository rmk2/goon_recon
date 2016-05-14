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

;; Find alliance for corporation

(define (corporation-to-alliance id)
  (cdr (assoc 'allianceID
	      (result->list
	       (string->xexpr
		(xml-api (string-append
			  api-root
			  "/corp/CorporationSheet.xml.aspx?corporationID="
			  (number->string id))))))))

(define (fill-alliance alliance corporation)
  (let ([try-corp (if (parse-corporation (car corporation))
		      (corporation-to-alliance (parse-corporation :id (car corporation)))
		      "0")])		      
    (cond
     [(not (string-empty? (car alliance))) (map string-upcase alliance)]
     [(and (string-empty? (car alliance)) (list? corporation))
      (if (or (false? try-corp) (zero? (string->number try-corp)))
	  sql-null
	  (parse-alliance :ticker try-corp))]
     [else sql-null])))

;; D-Scan munching

(define (AU->km n) (inexact->exact (* n 1.496e+8)))

(define (dscan-raw->list input)
  (map (lambda (x) (string-split x "\t"))
       (string-split input "\r\n")))

(define (dscan-normalise-distance lst)
  (filter-map (lambda (x)
		(match (last x)
		  [(pregexp #px"(.)\\skm$")
		   (list (first x)
			 (second x)
			 (string->number (string-join (regexp-match* #px"\\d+" (last x)) "")))]
		  [(pregexp #px"(.)\\sAU$")
		   (list (first x)
			 (second x)
			 (AU->km (string->number (car (regexp-match #px"[\\d.]+" (last x))))))]
		  [_ #f]))
	      lst))

(define (dscan-list->hash lst)
  (map (lambda (lst) (make-hash
		      (list
		       (cons 'name (first lst))
		       (cons 'type (second lst))
		       (cons 'distance (third lst)))))
       lst))

;; Sort by proximity, closest to furthest

(define (dscan-sort lst) (sort lst <= #:key (lambda (hash) (hash-ref hash 'distance))))

;; Type filters

(define (moon? lst)
  (filter (lambda (hash) (equal? "Moon" (hash-ref hash 'type))) lst))

(define (tower? lst)
  (filter (lambda (hash) (regexp-match? #px"(?i:control tower)" (hash-ref hash 'type))) lst))

(define (planet? lst)
  (filter (lambda (hash) (regexp-match? #px"^(?i:planet)" (hash-ref hash 'type))) lst))

(define (station? lst)
  (filter (lambda (hash) (regexp-match? #px"(?i:station)$" (hash-ref hash 'type))) lst))

;; Choose closest object (car), ideally after filtering
;; Example: (dscan-proximity (moon? lst))

(define (dscan-proximity lst)
  (if (empty? lst)
      #f
      (car (dscan-sort lst))))

;; Guess overall location

(define (dscan-parse-location lst)
  (map (lambda (name) (vector->list (parse-map name)))
       (filter-map (lambda (x) (if (dscan-proximity (x lst))
				   (hash-ref (dscan-proximity (x lst)) 'name)
				   #f))
		   (list moon? planet? station?))))

(define (dscan-guess-location lst)
  (let ([loc (dscan-parse-location lst)])
    (call-with-values (lambda ()
			(case (length loc)
			  [(1) loc]
			  [(2) (values (first loc) (second loc))]
			  [(3) (values (first loc) (second loc) (third loc))]
			  [(4) (values (first loc) (second loc) (third loc) (fourth loc))]
			  [else null]))
      set-intersect)))

;; Pretty-print location for HTML output

(define (pretty-print-location lst)
  (cond
   [(null? lst) "Unknown location"]
   [(= (length lst) 1)
    (let ([lst (flatten lst)])
      (string-join
       (list
   	(parse-region :name (sixth lst))
   	(parse-constellation :name (fifth lst))
   	(parse-solarsystem :name (fourth lst)))
       " › "))]
   [else
    (string-join
     (list
      (parse-region :name (first lst))
      (parse-constellation :name (second lst))
      (parse-solarsystem :name (third lst)))
     " › ")]))

;; d-scan -> moon scan data

(define (moonscan [input data] #:corporation [corporation poll_corporation] #:alliance [alliance poll_alliance])
  (if (< (hash-ref (dscan-proximity (moon? input)) 'distance) 10000)
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
	  (parse-type :id tower))))
      #f))

;; Pretty-print condensed d-scan result for HTML output

(define (pretty-print-dscan-result)
  (format "~a: ~a @ ~akm, belonging to ~a"
	  (hash-ref (dscan-proximity (moon? data)) 'name)
	  (parse-type :name (last moonscan-result))
	  (hash-ref (dscan-proximity (moon? data)) 'distance)
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
  (if (or (string-empty? (car poll_dscan))
	  (false? (dscan-proximity (tower? data)))
	  (false? (dscan-proximity (moon? data))))
      #f
      (let ([result (moonscan data)])
	(if (false? result)
	    #f
	    result))))

(when (not (false? moonscan-result))
  (sql-moon-update-scan (list moonscan-result)))

(output-http-headers)
(output-xml (doctype 'html))
(output-xml
 (html
  (head
   (title "Dashboard Scan Result"))
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
