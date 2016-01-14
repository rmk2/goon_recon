#! /usr/bin/env racket
#lang racket

(require eve)

;; XML options

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; Define API polls

(define query-limit (make-parameter 1000))

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

;; (define poll-test (api-charid (string-join
;; 				     '("Allister McGruffin"
;; 				       "Kameror"
;; 				       "Exarcheia"
;; 				       "Leon Bronshtein"
;; 				       "Peace Attack"
;; 				       "Juniper Leo"
;; 				       "Schwa Nuts"
;; 				       "Kathao Crendraven"
;; 				       "fawlty7"
;; 				       "Elo Knight")
;; 				     ",")))

(define (api-affiliation ids)
  (xml-api
   (string-append
    api-root
    "/eve/CharacterAffiliation.xml.aspx?ids="
    ids)))

;; (define poll-affiliation
;;   (rowset->hash
;;    (string->xexpr
;;     (api-affiliation
;;      (input-hash-join (rowset->hash (string->xexpr poll-test)) 'characterID)))))

;; Parse XML API data 

(define (parse-data lst)
  (map (lambda (hash) (list
		       (hash-ref hash 'characterName)
		       (hash-ref hash 'corporationName)
		       (hash-ref hash 'allianceName)))
       lst))

;; EDIS super data

(define edis
  (sort
   (map (lambda (lst)
	  (list
	   (list-ref lst 1)))
	polled-data)
   #:key car string-ci<?))

(define (edis-charid) (map (lambda (x) (api-charid (string-join x ","))) (split-list (flatten edis) 90)))

(define (edis-affiliation)
  (map (lambda (lst) (api-affiliation (input-hash-join (rowset->hash (string->xexpr lst)) 'characterID))) (edis-charid)))

(define (edis-result)
  (append-map (lambda (x) (rowset->hash (string->xexpr x))) (edis-affiliation)))

(define (edis-shiptype)
  (sort
   (map (lambda (lst)
	  (list
	   (list-ref lst 1)
	   (list-ref lst 0)))
	polled-data)
   #:key car string-ci<?))

(define (result-shiptype)
  (filter-map (lambda (hash ship) (if (string-ci=? (hash-ref hash 'characterName) (car ship))
				      (list
				       (second ship)
				       (hash-ref hash 'characterName)
				       (hash-ref hash 'corporationName)
				       (hash-ref hash 'allianceName))
				      #f))
	      (sort (edis-result) #:key (lambda (k) (hash-ref k 'characterName)) string-ci<?)
	      (edis-shiptype)))

(define (check-api)
  (for-each (lambda (y) (displayln (string-join y ","))) (result-shiptype)))

;; Execution

(check-api)
