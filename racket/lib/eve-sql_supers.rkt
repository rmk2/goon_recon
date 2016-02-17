#lang racket

(require db)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define (sql-create-supers)
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

(define (super-create-raw)
  (if (table-exists? sqlc "customSuperRaw")
      #t
      (query-exec sqlc "CREATE TABLE customSuperRaw ( shipTypeID INT NOT NULL, characterID INT NOT NULL, characterName VARCHAR(255) NOT NULL, corporationID INT NOT NULL, corporationName VARCHAR(255) NOT NULL, allianceID INT, allianceName VARCHAR(255), eventType VARCHAR(255), killID INT, moonID INT, systemID INT, regionID INT, datetime DATETIME )")))

(define (super-replace-killmails lst)
  (for-each (lambda (x)
	      (query sqlc "REPLACE INTO customSuperRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
		     (list-ref x 0)
		     (list-ref x 1)
		     (list-ref x 2)
		     (list-ref x 3)
		     (list-ref x 4)
		     (list-ref x 5)
		     (list-ref x 6)
		     (list-ref x 12)
		     (list-ref x 11)
		     (list-ref x 7)
		     (list-ref x 8)
		     (list-ref x 9)
		     (list-ref x 10)))
	    lst))

(define (super-create-view)
  (if (table-exists? sqlc "customSuperProcessed")
      #t
      (query-exec sqlc (string-append "CREATE VIEW customSuperProcessed AS "
				      "SELECT "
				      "invTypes.typeName,characterName,corporationName,allianceName,eventType,"
				      "killID,mapSolarSystems.solarSystemName,mapRegions.regionName,datetime "
				      "FROM customSuperRaw "
				      "LEFT JOIN invTypes ON invTypes.typeID = customSuperRaw.shipTypeID "
				      "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = customSuperRaw.systemID "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = customSuperRaw.regionID"))))
