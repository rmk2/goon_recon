#! /usr/bin/env racket
#lang racket

(require eve)

;; XML options

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; Define API polls

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

;; Execution

(define (edis)
  (remove-duplicates
   (map (lambda (lst)
	  (list-ref lst 1))
	(input-map-split (edis-data)))))

(define (edis-charid) (map (lambda (x) (api-charid (string-join x ","))) (split-list (edis) 100)))

(define (edis-affiliation)
  (map (lambda (lst) (api-affiliation (input-hash-join (rowset->hash (string->xexpr lst)) 'characterID))) (edis-charid)))

(define (edis-result)
  (append-map (lambda (x) (rowset->hash (string->xexpr x))) (edis-affiliation)))

(define (main)
  (for-each (lambda (y) (displayln (string-join y ","))) (parse-data (edis-result))))

(main)
