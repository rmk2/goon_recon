#! /usr/bin/env racket
#lang racket

(require srfi/19)
(require net/url)
(require file/gunzip)
;; (require web-server/private/gzip)
(require xml)
(require xml/path)

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; Define API polls

(define (xml-api str)
  (call/input-url (string->url str)
		  get-pure-port
		  (lambda (input) (call-with-output-string (lambda (x) (gunzip-through-ports input x))))
		  ;; (lambda (input) (gunzip/bytes (port->bytes input)))
		  '("Accept-Encoding: gzip" "User-Agent: ryko@rmk2.org")))

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

;; Secondary file handling

(define (unify-data)
  (let ([collected-file "/var/www/servers/eve.rmk2.org/pages/eve-intel_retroactive.txt"]
	[regions-file "/var/www/servers/eve.rmk2.org/pages/eve-intel_regions.txt"])
    (if (and (file-exists? collected-file) (file-exists? regions-file))
	(append (file->lines collected-file) (file->lines regions-file))
	(let ([collected "https://eve.rmk2.org/eve-intel_retroactive.txt"]
	      [regions "https://eve.rmk2.org/eve-intel_regions.txt"])
	  (append (call/input-url (string->url collected) get-pure-port port->lines)
		  (call/input-url (string->url regions) get-pure-port port->lines))))))

;; Parse XML API data 

(define (rowset->hash lst)
  (filter-map (lambda (x) (if (list? x)
			      (make-hash (map (lambda (y) (cons (car y) (cadr y))) (cadr x)))
			      #f))
	      (se-path*/list '(rowset) lst)))

(define (parse-data lst)
  (map (lambda (hash) (list
		       (hash-ref hash 'characterName)
		       (hash-ref hash 'corporationName)
		       (hash-ref hash 'allianceName)))
       lst))

(define (split-list [lst edis] [n 100])
  (let loop ([query lst] [limit n] [i 1] [result null])
    (if (<= (* i limit) (length query))
	(loop query limit (+ i 1) (list* (drop (take query (* i limit)) (* (- i 1) limit)) result))
	(reverse (list* (take-right query (remainder (length query) limit)) result)))))

;; Macros

(define-syntax input-hash-join ;; Convert polled hash data to CSV
  (syntax-rules ()
    ((_ hash key) (string-join (map (lambda (x) (hash-ref x key)) hash) ","))))

(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

(define-syntax input-map-join
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-join x ",")) input))))

;; Execution

;; (parse-data poll-affiliation)

(define edis
  (remove-duplicates
   (map (lambda (lst)
	  (list-ref lst 1))
	(input-map-split (unify-data)))))



(define (edis-charid) (map (lambda (x) (api-charid (string-join x ","))) (split-list edis 100)))

(define (edis-affiliation)
  (map (lambda (lst) (api-affiliation (input-hash-join (rowset->hash (string->xexpr lst)) 'characterID))) (edis-charid)))

(define (edis-result)
  (append-map (lambda (x) (rowset->hash (string->xexpr x))) (edis-affiliation)))

(define (main)
  (for-each (lambda (y) (displayln (string-join y ","))) (parse-data (edis-result))))
