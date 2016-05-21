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

(define (digest-update-affiliations lst)
  (begin (sql-corporation-update-affiliations lst)
	 lst))

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

;; Exec

(sql-corporation-update-affiliations
 (exec-limit-api-rate #:function map-corporation->alliance
		      #:input (map vector->values (sql-alliance-get-allianceids))
		      #:delay 4
		      #:limit 100))
