#!/usr/bin/env racket
#lang racket

(require eve)

(define (query-unknown-corporations)
  (query-rows sqlc "SELECT allianceTicker FROM moonScanView WHERE corporationName IS NULL"))

(define (extract-corporationids v)
  (filter-map (lambda (x) (cond
			   [(sql-null? x) #f]
			   [(eq? "-" x) #f]
			   [else x]))
	      (append-map vector->list v)))

(define (crest-poll-allianceid id)
  (json-api (string-append crest-root "/alliances/" (number->string id) "/")))

(define (parse-corporationids lst)
  (map (lambda (corp) (hash-ref corp 'id_str)) (hash-ref lst 'corporations)))

(define (hash-poll-corporations lst)
  (map (lambda (id) (make-hash
		     (result->list
		      (string->xexpr
		       (xml-api (string-append api-root "/corp/CorporationSheet.xml.aspx?corporationID=" id))))))
       lst))

(define (hash-parse-corporations lst)
  (map (lambda (corp) (list (hash-ref corp 'corporationID)
			    (hash-ref corp 'ticker)
			    (hash-ref corp 'corporationName)))
       lst))

(define (main-poll id)
  (exec-limit-api-rate #:function hash-poll-corporations
		       #:input (parse-corporationids (crest-poll-allianceid id))
		       #:delay 1
		       #:digest hash-parse-corporations
		       #:limit 30))

(define (main input)
  (for-each (lambda (alliance)
	      (begin
		(log-debug (format "[debug] Writing corporations for alliance ~s to database" alliance))
		(sql-corporation-update-corporations
		 (main-poll (parse-alliance :id alliance)))))
	    input))

;; Exec

(main (remove-duplicates (extract-corporationids (query-unknown-corporations))))
