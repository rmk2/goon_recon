#lang racket

(require "eve-api_tools.rkt")
(require "eve-string_tools.rkt")
(require "eve-sql_structs.rkt")
(require "eve-sql_types.rkt")

(require xml)
(require racket/set)
(require db)

(provide (all-defined-out))

;; Find alliance for corporation

(define (corporation-to-alliance id)
  (cdr (assoc 'allianceID
	      (result->list
	       (string->xexpr
		(xml-api (string-append
			  api-root
			  "/corp/CorporationSheet.xml.aspx?corporationID="
			  (number->string id))))))))

(define (fill-alliance #:alliance alliance #:corporation corporation)
  (let* ([corporation (if (string? corporation) (string-upcase corporation) "")]
	 [try-corp (if (parse-corporation corporation)
		      (corporation-to-alliance (parse-corporation :id corporation))
		      "0")])		      
    (cond
     [(not (string-empty? alliance)) (string-upcase alliance)]
     [(and (string-empty? alliance) (string? corporation)
	   (not (false? try-corp)) (not (zero? (string->number try-corp))))
      (parse-alliance :ticker try-corp)]
     [else ""])))

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

;; Translate a list of dscan hashes into a list of dscan structs

(define (dscan-hash->struct lst)
  (map (lambda (hash) (call-with-values
			 (lambda ()
			   (values (hash-ref hash 'name)
				   (hash-ref hash 'type)
				   (hash-ref hash 'distance)))
			dscan))
       lst))

;; Sort by proximity, closest to furthest

(define (dscan-sort lst) (sort lst <= #:key (lambda (hash) (hash-ref hash 'distance))))

;; Type filters

(define (moon? lst)
  (filter (lambda (hash) (equal? "Moon" (hash-ref hash 'type))) lst))

(define (sun? lst)
  (filter (lambda (hash) (regexp-match? #px"^(?i:sun)" (hash-ref hash 'type))) lst))

(define (tower? lst)
  (filter (lambda (hash) (regexp-match? #px"(?i:control tower)" (hash-ref hash 'type))) lst))

(define (planet? lst)
  (filter (lambda (hash) (regexp-match? #px"^(?i:planet)" (hash-ref hash 'type))) lst))

(define (station? lst)
  (filter (lambda (hash) (regexp-match? #px"(?i:station)$" (hash-ref hash 'type))) lst))

(define (forcefield? lst)
  (filter (lambda (hash) (regexp-match? #px"^(?i:force field)" (hash-ref hash 'type))) lst))

(define (citadel? lst)
  (filter (lambda (hash) (member (hash-ref hash 'type)
				 (map (lambda (x) (vector-ref x 2)) (parse-type :members 1657)))) lst))

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
		   (list moon? planet? station? sun?))))

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

;; Transform guess into a consistent form
;; (region -> constellation -> system)

(define (guess->location lst)
  (cond
   [(null? lst) null]
   [(= (length lst) 1)
    (let ([lst (flatten lst)])
      (list
       (sixth lst)
       (fifth lst)
       (fourth lst)))]
   [else
    (list
     (first lst)
     (second lst)
     (third lst))]))

;; Pretty-print location for HTML output

(define (pretty-print-location lst)
  (cond
   [(null? lst) "Unknown location"]
   [else
    (string-join
     (list
      (parse-region :name (first lst))
      (parse-constellation :name (second lst))
      (parse-solarsystem :name (third lst)))
     " › ")]))
