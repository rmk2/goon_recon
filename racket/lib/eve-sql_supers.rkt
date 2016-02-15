#lang racket

(require db)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define (eve-create-tables)
  (begin
    (if (table-exists? sqlc "customSuperAffiliations")
	#t
	(query-exec sqlc "CREATE TABLE customSuperAffiliations ( characterID INT NOT NULL, characterName VARCHAR(255) NOT NULL, corporationID INT NOT NULL, corporationName VARCHAR(255) NOT NULL, allianceID INT, allianceName VARCHAR(255), PRIMARY KEY (characterID) )"))
    (if (table-exists? sqlc "customSuperKillmails")
	#t
	(query-exec sqlc "CREATE TABLE customSuperKillmails ( characterName VARCHAR(255) NOT NULL, corporationName VARCHAR(255) NOT NULL, allianceName VARCHAR(255), shipType VARCHAR(255), system VARCHAR(255), region VARCHAR(255), date DATE, PRIMARY KEY (characterName) )"))))

(define (sql-replace-characters lst)
  (for-each (lambda (x)
	      (query sqlc "REPLACE INTO customSuperAffiliations VALUES (?, ?, ?, ?, ?, ?)"
		     (first x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)))
	    lst))

(define (sql-replace-killmails lst)
  (for-each (lambda (x)
	      (query sqlc "REPLACE INTO customSuperKillmails VALUES (?, ?, ?, ?, ?, ?, ?)"
		     (second x)
		     (third x)
		     (fourth x)
		     (first x)
		     (fifth x)
		     (sixth x)
		     (seventh x)))
	    lst))

(define (sql-create-view)
  (if (table-exists? sqlc "customSuperView")
      #t
      (query-exec sqlc
	     (string-append
	      "CREATE VIEW customSuperView AS "
	      "SELECT customSuperKillmails.shiptype,customSuperKillmails.characterName,"
	      "customSuperAffiliations.corporationName,customSuperAffiliations.allianceName,"
	      "customSuperKillmails.system,customSuperKillmails.region,customSuperKillmails.date "
	      "FROM customSuperKillmails "
	      "INNER JOIN customSuperAffiliations "
	      "ON customSuperKillmails.characterName=customSuperAffiliations.characterName"))))

(define (sql-filter-watchlist)
  (query-rows sqlc "SELECT characterName,shipType,allianceName FROM customSuperView ORDER BY date,shipType ASC"))
