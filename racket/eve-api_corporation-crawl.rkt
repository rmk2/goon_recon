#!/usr/bin/env racket
#lang racket

(require eve)

;; Database handling

(define (sql-corporation-create-affiliations)
  (if (table-exists? sqlc "customCorporationAffiliations")
      #t
      (query-exec sqlc "CREATE TABLE customCorporationAffiliations ( corporationID INT NOT NULL, allianceID INT, datetime DATETIME DEFAULT '0000-00-00 00:00:00', PRIMARY KEY (corporationID) )")))

(define (sql-corporation-update-affiliations lst)
  (for-each (lambda (x) (query sqlc (string-append "INSERT INTO customCorporationAffiliations VALUES (?, ?, ?) "
						   "ON DUPLICATE KEY UPDATE allianceID=?,datetime=?")
			       (first x)
			       (second x)
			       (third x)
			       (second x)
			       (third x)))
	    lst))

(define (sql-alliance-get-allianceids)
  (query-rows sqlc "SELECT allianceID FROM customAlliances"))

(define (sql-corporation-get-affiliations)
  (query-rows sqlc (string-append "SELECT main.corporationID,c.corporationName,main.allianceID,a.allianceName,main.datetime "
				  "FROM customCorporationAffiliations as main "
				  "LEFT JOIN customCorporations AS c ON main.corporationID = c.corporationID "
				  "LEFT JOIN customAlliances AS a ON main.allianceID = a.allianceID "
				  "ORDER BY allianceName")))

(define (digest-update-affiliations lst [i null])
  (begin (sql-corporation-update-affiliations lst)
	 (log-debug (format "[debug] Saved associations to database"))
	 (list (null? i) "written" i)))

;; Get CREST alliance data for id

(define (crest-poll-allianceid id)
  (json-api (string-append crest-root "/alliances/" (number->string id) "/")))

;; Parse corporationIDs from alliance API polls

;; (define (parse-corporationid_str lst)
;;   (map (lambda (corp) (hash-ref corp 'id_str)) (hash-ref lst 'corporations)))

(define (parse-corporationids lst)
  (map (lambda (corp) (hash-ref corp 'id)) (hash-ref lst 'corporations)))

;; Map alliance information into database-ready format
;; (corporationID allianceID datetime)

(define (corporation->alliance id)
  (map (lambda (x) (list x id (srfi-date->sql-timestamp (current-date))))
       (parse-corporationids (crest-poll-allianceid id))))

(define (map-corporation->alliance lst)
  (append-map (lambda (id) (let ([poll (corporation->alliance id)])
			     (if (empty? poll) null poll)))
	      lst))

;; Continuation

(define (sql-alliance-max-allianceid)
  (query-value sqlc "SELECT MAX(allianceID) FROM customAlliances"))

(define (sql-alliance-min-allianceid)
  (query-value sqlc "SELECT MIN(allianceID) FROM customAlliances"))

(define (sql-corporation-max-allianceid)
  (query-value sqlc "SELECT MAX(allianceID) FROM customCorporationAffiliations"))

;; Exec

(define (main start-id)
  (sql-corporation-update-affiliations
   (exec-limit-api-rate #:function map-corporation->alliance
			#:input (member start-id (map vector->values (sql-alliance-get-allianceids)))
			#:delay 2
			#:digest digest-update-affiliations
			#:limit 20)))

(cond [(< (sql-corporation-max-allianceid) (sql-alliance-max-allianceid))
       (main (sql-corporation-max-allianceid))]
      [else (main (sql-alliance-min-allianceid))])
