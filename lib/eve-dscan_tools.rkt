#lang racket

(require "eve-api_tools.rkt")
(require "eve-list_tools.rkt")
(require "eve-string_tools.rkt")
(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")
(require "eve-sql_types.rkt")

(require xml)
(require racket/set)
(require db)
(require db/util/datetime)
(require srfi/19)

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
(define (m->km n) (exact->inexact (/ n 1000)))

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
		  [(pregexp #px"(.)\\sm$")
		   (list (first x)
			 (second x)
			 (m->km (string->number (string-join (regexp-match* #px"\\d+" (last x)) ""))))]
		  [(pregexp #px"(.)\\sAU$")
		   (list (first x)
			 (second x)
			 (AU->km (string->number (car (regexp-match #px"[\\d.]+" (string-replace (last x) "," "."))))))]
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

(define (stargate? lst)
  (filter (lambda (hash) (regexp-match? #px"^(?i:stargate)" (hash-ref hash 'type))) lst))

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

;; Create view that combines typeID/Name, groupID/Name and categoryID

(define (sql-type-create-associations)
  (if (table-exists? sqlc "typeAssociations")
      #t
      (query-exec sqlc "CREATE VIEW typeAssociations AS SELECT t.typeID,t.typeName,g.groupID,g.groupName,g.categoryID FROM invGroups AS g LEFT JOIN invTypes AS t ON t.groupID = g.groupID WHERE t.published = 1")))

;; Type associations -> typeAssociation struct

(define (sql-type-parse-association category)
  (query-rows sqlc "SELECT typeID,typeName,groupID,groupName,categoryID FROM typeAssociations WHERE categoryID = ?" category))

(define (type-association-list category)
  (map (lambda (v) (sql-parse->struct v #:struct typeAssociation))
       (sql-type-parse-association category)))

;; Type association shortcuts

;; (define deployable-type-list (type-association-list 22))
;; (define drone-type-list (type-association-list 18))
;; (define fighter-type-list (type-association-list 87))
;; (define ship-type-list (type-association-list 6))
;; (define sovereignty-type-list (type-association-list 40))
;; (define starbase-type-list (type-association-list 23))
;; (define structure-type-list (type-association-list 65))

;; Parse dscan data, filter by chosen type association list

(define (parse-type-list input type-list)
  (filter-map (lambda (hash)
		(findf (lambda (x) (equal? (hash-ref hash 'type) (typeAssociation-typename x))) type-list))
	      input))

(define-syntax filter-dscan
  (syntax-rules (:deployable :drone :fighter :ship :sovereignty :starbase :structure)
    ((_ :deployable input) (filter-dscan input (type-association-list 22)))
    ((_ :drone input) (filter-dscan input (type-association-list 18)))
    ((_ :fighter input) (filter-dscan input (type-association-list 87)))
    ((_ :ship input) (filter-dscan input (type-association-list 6)))
    ((_ :sovereignty input) (filter-dscan input (type-association-list 40)))
    ((_ :starbase input) (filter-dscan input (type-association-list 23)))
    ((_ :structure input) (filter-dscan input (type-association-list 65)))
    ((_ input type-list)
     (let ([result (parse-type-list input type-list)])
       (cond
	[(empty? result) null]
	[else (cons
	       (sort (count-duplicates (sort (map (lambda (x) (typeAssociation-typename x)) result) string-ci>=?))
		     >=
		     #:key second)
	       (sort (count-duplicates (sort (map (lambda (x) (typeAssociation-groupname x)) result) string-ci>=?))
		     >=
		     #:key second))])))))

;; Split moon probing results per moon (use dscan-raw->list as input)

(define (goo-split-probe-results lst)
  (let loop ([data lst] [result null])
    (cond [(empty? data) result]
	  [else (loop (dropf (cdr data) (lambda (x) (not (list? (split-moon-display (car x))))))
		      (list* (cons (first data)
				   (takef (cdr data) (lambda (x) (not (list? (split-moon-display (car x)))))))
			     result))])))

;; Parse moon probing results into a list per type per moon

(define (goo-parse-results lst)
  (append-map (lambda (x)
		(let* ([data x]
		       [location (sql-parse->struct (parse-map (car (first data))) #:struct mapDenormalize)]
		       [moon (list
			      (mapDenormalize-region location)
			      (mapDenormalize-constellation location)
			      (mapDenormalize-system location)
			      (cdr (split-moon-display (car (first data))))
			      (srfi-date->sql-timestamp (current-date)))])
		  (map (lambda (x) (flatten (list moon (parse-type :id (car x)) (cadr x)))) (cdr data))))
	      lst))

;; Transform a list of moon lists into a list of sql-goo structs

(define (goo-list->struct lst)
  (map (lambda (moon) (call-with-values (lambda () (vector->values (list->vector moon))) sql-goo))
       lst))
