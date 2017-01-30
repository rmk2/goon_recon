#!/usr/bin/env racket
#lang racket

(require eve)

(provide query-scan-unknown-corporations
	 query-kill-unknown-corporations
	 query-input-unknown-corporations
	 extract-corporationids
	 crest-poll-allianceid
	 parse-corporationids
	 scan-unknown
	 general-unknown-poll)

;; Database

(define (query-scan-unknown-corporations)
  (flatten
   (filter-not empty?
	       (list*
		(query-list sqlc "SELECT allianceTicker FROM moonScanView WHERE corporationName IS NULL")
		(query-list sqlc "SELECT allianceTicker FROM citadelScanView WHERE corporationName IS NULL")))))

(define (query-kill-unknown-corporations)
  (flatten
   (filter-not empty?
	       (list*
		(query-list sqlc "SELECT towerKillRaw.corporationID FROM towerKillRaw LEFT JOIN customCorporations ON customCorporations.corporationID = towerKillRaw.corporationID WHERE corporationName IS NULL")
		(query-list sqlc "SELECT citadelKillRaw.corporationID FROM citadelKillRaw LEFT JOIN customCorporations ON customCorporations.corporationID = citadelKillRaw.corporationID WHERE corporationName IS NULL")))))

(define (query-input-unknown-corporations)
  (query-list sqlc "SELECT corporationID FROM customCorporationInput"))

;; Extract corporationIDs from CREST allianceSheet

(define (extract-corporationids v)
  (filter-map (lambda (x) (cond
			   [(sql-null? x) #f]
			   [(eq? "-" x) #f]
			   [(string-empty? x) #f]
			   [else x]))
	      v))

(define (crest-poll-allianceid id)
  (cond [(string-empty? id) null]
	[else (json-api (string-append crest-root "/alliances/" (number->string id) "/"))]))

(define (parse-corporationids lst)
  (map (lambda (corp) (hash-ref corp 'id_str)) (hash-ref lst 'corporations)))

;; Update polled towers from moonScanRaw

(define (scan-unknown-poll id)
  (exec-limit-api-rate #:function hash-poll-corporations
		       #:input (parse-corporationids (crest-poll-allianceid id))
		       #:delay 1
		       #:digest hash-parse-corporations
		       #:limit 30))

(define (scan-unknown input)
  (for-each (lambda (alliance)
	      (begin
		(log-debug (format "[debug] Writing corporations for alliance ~s to database" alliance))
		(sql-corporation-update-corporations
		 (scan-unknown-poll (parse-alliance :id alliance)))))
	    input))

;; Update polled towers from towerKillRaw

(define (general-unknown-poll lst)
  (exec-limit-api-rate #:function hash-poll-corporations
		       #:input (map number->string lst)
		       #:delay 1
		       #:digest hash-parse-corporations
		       #:limit 30))

;; Exec

(scan-unknown (remove-duplicates (extract-corporationids (query-scan-unknown-corporations))))

(sql-corporation-update-corporations (general-unknown-poll (remove-duplicates (query-kill-unknown-corporations))))

(sql-corporation-update-corporations (general-unknown-poll (remove-duplicates (query-input-unknown-corporations))))
