#lang racket

(require "eve-api_tools.rkt")
(require "eve-list_tools.rkt")
(require "eve-string_tools.rkt")
(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")

(require db)
(require db/util/datetime)
(require srfi/19)
(require racket/future)
(require (only-in racket/date
		  date->seconds))

(provide (all-defined-out))

;; SQL

(define (sql-character-create-raw)
  (if (table-exists? sqlc "customCharacters")
      #t
      (query-exec sqlc "CREATE TABLE customCharacters ( characterID INT NOT NULL, characterName VARCHAR(255) NOT NULL, corporationID INT, corporationName VARCHAR(255), allianceID INT, allianceName VARCHAR(255), datetime DATETIME, KEY ( characterName ), PRIMARY KEY ( characterID ) )")))

(define (sql-character-update-ids lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO customCharacters VALUES (?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE characterName=?,corporationID=?,corporationName=?,allianceID=?,allianceName=?,datetime=?"
		     (sql-character-id x)
		     (sql-character-name x)
		     (sql-character-corporationid x)
		     (sql-character-corporation x)
		     (sql-character-allianceid x)
		     (sql-character-alliance x)
		     (sql-character-datetime x)
		     (sql-character-name x)
		     (sql-character-corporationid x)
		     (sql-character-corporation x)
		     (sql-character-allianceid x)
		     (sql-character-alliance x)
		     (sql-character-datetime x)))
	    lst))

(define-syntax  sql-parse-character
  (syntax-rules (:id :name :datetime)
    ((_ arg)
     (cond
      [(number? arg)
       (query-maybe-row sqlc "SELECT characterID,characterName,corporationID,corporationName,allianceID,allianceName,datetime FROM customCharacters WHERE characterID = ?" arg)]
      [(string? arg)
       (query-maybe-row sqlc "SELECT characterID,characterName,corporationID,corporationName,allianceID,allianceName,datetime FROM customCharacters WHERE characterName = ?" arg)]))
    ((_ :id arg) (vector-ref (sql-parse-character arg) 0))
    ((_ :name arg) (vector-ref (sql-parse-character arg) 1))))

(define (sql-character-maybe-ids lst #:difference [difference 7200])
  (define (sql-character-outdated? query diff)
    (< diff
       (- (current-seconds)
	  (date->seconds (sql-datetime->srfi-date query)))))
  (map (lambda (name) (let ([query (sql-parse-character name)])
			(cond [(false? query) name]
			      [(sql-character-outdated? (vector-ref (vector-take-right query 1) 0) difference)
			       (hasheq 'characterID (number->string (vector-ref query 0)) 'name (vector-ref query 1))]
			      [else (sql-parse->struct query #:struct sql-character)])))
       lst))

;; hash-poll-affiliation <-> sql

(define (map-character-hash->struct lst)
  (map (lambda (p) (call-with-values
		       (lambda ()
			 (values
			  (hash-ref p 'characterID)
			  (hash-ref p 'characterName)
			  (hash-ref p 'corporationID)
			  (hash-ref p 'corporationName)
			  (hash-ref p 'allianceID)
			  (hash-ref p 'allianceName)
			  (srfi-date->sql-timestamp (current-date))))
		     sql-character))
       lst))

;; Parsing

(define (map-hash-parse-unknown lst)
  (define (poll-charid-helper lst)
    (exec-limit-api-rate #:function hash-poll-characterids
			 #:input lst
			 #:delay 1
			 #:limit 1500))
  (let* ([query (sql-character-maybe-ids lst)]
	 [known (filter sql-character? query)]
	 [outdated (filter hash? query)]
	 [unknown (filter string? query)]
	 [lookup (if (empty? unknown)
		     null
		     (flatten
		      (for/list ([i (split-list unknown (round (/ (length unknown) 2)))])
			(touch (future (lambda () (poll-charid-helper i)))))))])
    (list known (append outdated lookup))))

;; Count duplicate corporations/alliances

(define (count-affiliations lst)
  (sort (map (lambda (x) (cond
			  [(string-empty? (car x)) (list "-" (cadr x))]
			  [else x]))
	     (filter (lambda (x) (not (string-empty? (car x))))
		     (count-duplicates (sort lst string-ci>?))))
	>=
	#:key second))