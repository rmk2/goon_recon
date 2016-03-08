#lang racket

(require db)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define (sql-super-create-affiliations)
  (if (table-exists? sqlc "intelSuperAffiliations")
      #t
      (query-exec sqlc "CREATE TABLE intelSuperAffiliations ( characterID INT NOT NULL, characterName VARCHAR(255) NOT NULL, corporationID INT NOT NULL, corporationName VARCHAR(255) NOT NULL, allianceID INT, allianceName VARCHAR(255), PRIMARY KEY (characterID) )")))

(define (sql-super-update-affiliations lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO intelSuperAffiliations VALUES (?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE characterName=?,corporationID=?,corporationName=?,allianceID=?,allianceName=?"
		     (first x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)))
	    lst))

(define (sql-super-get-characterids)
  (query-list sqlc "SELECT characterID FROM intelSuperAffiliations"))

(define (sql-super-populate-affiliations)
  (query-exec sqlc "INSERT IGNORE INTO intelSuperAffiliations( characterID,characterName,corporationID,corporationName,allianceID,allianceName ) SELECT DISTINCT characterID,characterName,corporationID,corporationName,allianceID,allianceName FROM intelSuperRaw"))

(define (sql-super-create-raw)
  (if (table-exists? sqlc "intelSuperRaw")
      #t
      (query-exec sqlc "CREATE TABLE intelSuperRaw ( shipTypeID INT NOT NULL, characterID INT NOT NULL, characterName VARCHAR(255) NOT NULL, corporationID INT NOT NULL, corporationName VARCHAR(255) NOT NULL, allianceID INT, allianceName VARCHAR(255), eventType VARCHAR(255), killID INT, victimTypeID INT, moonID INT, systemID INT, regionID INT, datetime DATETIME, UNIQUE KEY (characterID, killID) )")))

(define (sql-super-insert-killmails lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT IGNORE INTO intelSuperRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
		     (list-ref x 0)
		     (list-ref x 1)
		     (list-ref x 2)
		     (list-ref x 3)
		     (list-ref x 4)
		     (list-ref x 5)
		     (list-ref x 6)
		     (last x)
		     (list-ref x 11)
		     (list-ref x 12)
		     (list-ref x 7)
		     (list-ref x 8)
		     (list-ref x 9)
		     (list-ref x 10)))
	    lst))

(define (sql-super-create-view)
  (if (table-exists? sqlc "intelSuperView")
      #t
      (query-exec sqlc (string-append "CREATE VIEW intelSuperView AS "
				      "SELECT "
				      "shipTypes.typeName AS shipTypeName,characterName,corporationName,allianceName,eventType,"
				      "victimTypes.typeName AS victimTypeName,killID,mapSolarSystems.solarSystemName,"
				      "mapRegions.regionName,datetime "
				      "FROM intelSuperRaw "
				      "LEFT JOIN invTypes AS shipTypes ON shipTypes.typeID = intelSuperRaw.shipTypeID "
				      "LEFT JOIN invTypes AS victimTypes ON victimTypes.typeID = intelSuperRaw.victimTypeID "
				      "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = intelSuperRaw.systemID "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = intelSuperRaw.regionID"))))

;; Backwards compatibility

(define (super-replace-killmails lst) (sql-super-insert-killmails lst))

(define (sql-filter-watchlist)
  (query-rows sqlc (string-append "SELECT affiliations.characterName,view.shipTypeName,affiliations.allianceName "
				  "FROM intelSuperAffiliations AS affiliations "
				  "INNER JOIN intelSuperView AS view ON affiliations.characterName = view.characterName")))
