#lang racket

(require db)
(require xml)

(require "eve-sql_main.rkt")
(require "eve-api_tools.rkt")

(provide (all-defined-out))

(define (api-fetch-alliances)
  (rowset->hash
   (string->xexpr
    (xml-api "https://api.eveonline.com/eve/AllianceList.xml.aspx?version=1"))))

(define (sql-create-alliances)
  (if (table-exists? sqlc "customAlliances")
      #t
      (query-exec sqlc "CREATE TABLE customAlliances ( allianceID INT NOT NULL, allianceTicker VARCHAR(5) NOT NULL, allianceName VARCHAR(255) NOT NULL, PRIMARY KEY ( allianceID ), UNIQUE KEY ( allianceTicker ) )")))

(define (sql-replace-alliances lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO customAlliances VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE allianceID=?,allianceTicker=?,allianceName=?"
		     (hash-ref x 'allianceID)
		     (hash-ref x 'shortName)
		     (hash-ref x 'name)
		     (hash-ref x 'allianceID)
		     (hash-ref x 'shortName)
		     (hash-ref x 'name)))
	    lst))

(define (sql-query-alliances)
  (map vector->list (query-rows sqlc "SELECT allianceID,allianceName,allianceTicker FROM customAlliances")))

(define (sql-alliance-get-allianceids)
  (query-rows sqlc "SELECT allianceID FROM customAlliances"))
