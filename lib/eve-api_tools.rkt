#lang racket

(require json)
(require xml)
(require xml/path)
(require net/url)
(require file/gunzip)
(require xexpr-path)

(require "eve-list_tools.rkt")

(provide (all-defined-out))

;; Generic CREST (json) API polling function; output: jsexpr

(define-syntax json-api
  (syntax-rules (:gzip :plain)
    ((_ :plain str)
     (bytes->jsexpr
      (call/input-url (string->url str)
		      get-pure-port
		      port->bytes
		      '("User-Agent: ryko@rmk2.org"))))
    ((_ :gzip str)
     (bytes->jsexpr
      (call/input-url (string->url str)
		      get-pure-port
		      (lambda (input) (call-with-output-bytes (lambda (x) (gunzip-through-ports input x))))
		      '("Accept-Encoding: gzip" "User-Agent: ryko@rmk2.org"))))
    ((_ str) (json-api :gzip str))))

;; CREST: root address

(define crest-root "https://crest-tq.eveonline.com")

;; Generic APIv2 (xml) API polling function; output: string

(define-syntax xml-api
  (syntax-rules (:gzip :plain)
    ((_ :plain str)
     (call/input-url (string->url str)
		     get-pure-port
		     port->string
		     '("User-Agent: ryko@rmk2.org")))
    ((_ :gzip str)
     (call/input-url (string->url str)
		     get-pure-port
		     (lambda (input) (call-with-output-string (lambda (x) (gunzip-through-ports input x))))
		     '("Accept-Encoding: gzip" "User-Agent: ryko@rmk2.org")))
    ((_ str) (xml-api :plain str))))

;; Extract XML APIv2 response bodies

(define (rowset->hash lst)
  (filter-map (lambda (x) (if (list? x)
			      (make-hash (map (lambda (y) (cons (car y) (cadr y))) (cadr x)))
			      #f))
	      (se-path*/list '(rowset) lst)))

(define (result->list lst)
  (filter-map (lambda (x) (cond
			   [(member 'rowset x) #f]
			   [(member 'attributes x) #f]
			   [else (cons (first x) (last x))]))
	      (filter list? (se-path*/list '(result) lst))))

(define (rowset->list lst)
  (map (lambda (row) (cadr row))
       (xexpr-path-list '(result rowset row) lst)))

(define (rowset-name->list key lst)
  (cons (string->symbol key)
	(list
	 (map (lambda (row) (cadr row))
	      (xexpr-path-list `(result rowset (name ,key) row) lst)))))

;; Convert specified hash content into csv data

(define-syntax input-hash-join 
  (syntax-rules ()
    ((_ hash key) (string-join (map (lambda (x) (hash-ref x key)) hash) ","))))

;; Extract characterIDs from a list of hashes (containing characterID & characterName)

(define (map-hash-extract lst [key 'characterID])
  (map (lambda (hash) (hash-ref hash key))
       lst))

;; XML APIv2: root address

(define api-root "https://api.eveonline.com")

;; XML APIv2: Get a list of characterIDs from a list of characterNames

(define (api-charid names)
  (xml-api (string-append
	    api-root
	    "/eve/CharacterID.xml.aspx?names="
	    names)))

;; XML APIv2: Get a list of affiliations from a list of characterIDs

(define (api-affiliation ids)
  (xml-api (string-append
	    api-root
	    "/eve/CharacterAffiliation.xml.aspx?ids="
	    ids)))

;; XML APIv2: translate names -> characterIDs, output: list of hashes

(define (hash-poll-characterids lst)
  (append-map (lambda (str) (rowset->hash (string->xexpr str)))
	      (map (lambda (x) (api-charid (string-join x ",")))
		   (split-list (flatten lst) (chunk-size)))))

;; XML APIv2: translate characterIDs -> affiliations, output: list of hashes

(define (hash-poll-affiliation lst)
  (append-map (lambda (str) (rowset->hash (string->xexpr str)))
	      (map (lambda (id-list) (api-affiliation (string-join id-list ",")))
		   (split-list (flatten lst) (chunk-size)))))

;; XML APIv2: translate corporationID -> corporationSheet, output: hash

(define (hash-poll-corporations lst)
  (map (lambda (id) (make-hash
		     (result->list
		      (string->xexpr
		       (xml-api (string-append api-root "/corp/CorporationSheet.xml.aspx?corporationID=" id))))))
       lst))

;; XML APIv2: extract characterID/Name,corporationID/Name,allianceID/Name, output: list

(define (map-hash-parse-affiliation lst)
  (map (lambda (hash) (list
		       (hash-ref hash 'characterID)
		       (hash-ref hash 'characterName)
		       (hash-ref hash 'corporationID)
		       (hash-ref hash 'corporationName)
		       (hash-ref hash 'allianceID)
		       (hash-ref hash 'allianceName)))
       lst))

;; XML APIv2: extract corporationID,Ticker,corporationName from corporationSheet, output: list

(define (hash-parse-corporations lst)
  (map (lambda (corp) (list (hash-ref corp 'corporationID)
			    (hash-ref corp 'ticker)
			    (hash-ref corp 'corporationName)))
       lst))

;; Run a function with rate limiting, defined by (query-limit), with x second delay
;; (query-limit) requests per second with (chunk-size) list items per request

(define chunk-size (make-parameter 90))
(define query-limit (make-parameter 2500))

(define (exec-limit-api-rate #:function function
			     #:input lst
			     #:delay [delay 1]
			     #:digest [digest null]
			     #:limit [limit (query-limit)])
  (if (<= (length lst) limit)
      (if (null? digest)
	  (function lst)
	  (digest (function lst)))
      (let loop ([data (split-list lst limit)] [i 0] [result '()])
	(if (< i (length data))
	    (begin
	      (when (> i 0) (begin (log-debug (format "[debug] Waiting before next iteration: ~s seconds" delay))
				   (sleep delay)))
	      (log-debug (format "[debug] Current iteration (inner loop, ~s items each): ~s of ~s" limit (+ i 1) (length data)))
	      (if (null? digest)
		  (loop data (+ i 1) (append (function (list-ref data i)) result))
		  (loop data (+ i 1) (append (digest (function (list-ref data i))) result))))
	    result))))
