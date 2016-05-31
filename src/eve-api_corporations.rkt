#!/usr/bin/env racket
#lang racket

(require eve)

;; Database

(define (query-scan-unknown-corporations)
  (query-rows sqlc "SELECT allianceTicker FROM moonScanView WHERE corporationName IS NULL"))

(define (query-kill-unknown-corporations)
  (query-rows sqlc "SELECT towerKillRaw.corporationID FROM towerKillRaw LEFT JOIN customCorporations ON customCorporations.corporationID = towerKillRaw.corporationID WHERE corporationName IS NULL"))

;; Extract corporationIDs from CREST allianceSheet

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

(define (tower-unknown-poll lst)
  (exec-limit-api-rate #:function hash-poll-corporations
		       #:input (map (lambda (x) (number->string (vector->values x))) lst)
		       #:delay 1
		       #:digest hash-parse-corporations
		       #:limit 30))

;; Exec

(scan-unknown (remove-duplicates (extract-corporationids (query-scan-unknown-corporations))))

(sql-corporation-update-corporations (tower-unknown-poll (query-kill-unknown-corporations)))
