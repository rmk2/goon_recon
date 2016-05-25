#lang racket

(require db)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define (sql-corporation-update-corporations lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT IGNORE customCorporations VALUES (?, ?, ?)"
		     (first x)
		     (second x)
		     (third x)))
	    lst))

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

(define (sql-corporation-get-affiliations)
  (query-rows sqlc (string-append "SELECT main.corporationID,c.corporationName,main.allianceID,a.allianceName,main.datetime "
				  "FROM customCorporationAffiliations as main "
				  "LEFT JOIN customCorporations AS c ON main.corporationID = c.corporationID "
				  "LEFT JOIN customAlliances AS a ON main.allianceID = a.allianceID "
				  "ORDER BY allianceName")))
