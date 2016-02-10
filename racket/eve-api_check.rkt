#! /usr/bin/env racket
#lang racket

(require eve)

(provide (except-out (all-defined-out)
		     edis-shiptype
		     result-shiptype
		     print-result-shiptype))
		     
;; XML options

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; Define API polls

(define query-limit (make-parameter 5000))

(define polled-data (let ([lst (unique-car (input-map-split (edis-data)) second)])
		      (if (<= (length lst) (query-limit))
			  lst
			  (take-right lst (query-limit)))))

(define api-root "https://api.eveonline.com")

(define (api-charid names)
  (xml-api (string-append
	    api-root
	    "/eve/CharacterID.xml.aspx?names="
	    names)))

(define (api-affiliation ids)
  (xml-api
   (string-append
    api-root
    "/eve/CharacterAffiliation.xml.aspx?ids="
    ids)))

;; Parse XML API data 

(define (parse-data lst)
  (map (lambda (hash) (list
		       (hash-ref hash 'characterID)
		       (hash-ref hash 'characterName)
		       (hash-ref hash 'corporationID)
		       (hash-ref hash 'corporationName)
		       (hash-ref hash 'allianceID)
		       (hash-ref hash 'allianceName)))
       lst))

;; EDIS super data

(define-syntax edis-list
  (syntax-rules (:shiptype)
    ((_ lst) (map (lambda (l) (list (list-ref l 1))) lst))
    ((_ :shiptype lst) (map (lambda (l) (cons (list-ref l 1)
					      (list-ref l 0)))
			    lst))))

(define edis (edis-list polled-data))
(define edis-shiptype (edis-list :shiptype polled-data))

(define (api-check-result lst)
  (append-map (lambda (x) (rowset->hash (string->xexpr x)))
	      (map (lambda (lst) (api-affiliation (input-hash-join (rowset->hash (string->xexpr lst)) 'characterID)))
		   (map (lambda (x) (api-charid (string-join x ","))) (split-list (flatten lst) 90)))))

(define (result-shiptype)
  (filter-map (lambda (hash) (if (assoc (hash-ref hash 'characterName) edis-shiptype)
				 (list
				  (cdr (assoc (hash-ref hash 'characterName) edis-shiptype))
				  (hash-ref hash 'characterName)
				  (hash-ref hash 'corporationName)
				  (hash-ref hash 'allianceName))
				 #f))
	      (edis-result)))

(define (print-result-shiptype)
  (for-each (lambda (y) (displayln (string-join y ","))) (result-shiptype)))

;; Execution
