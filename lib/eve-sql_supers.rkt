#lang racket

(require db)
(require db/util/datetime)

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")

(provide (all-defined-out))

(define (sql-super-create-affiliations)
  (if (table-exists? sqlc "intelSuperAffiliations")
      #t
      (query-exec sqlc "CREATE VIEW intelSuperAffiliations AS SELECT DISTINCT c.characterID,c.characterName,c.corporationID,c.corporationName,c.allianceID,c.allianceName FROM intelSuperRaw AS s LEFT JOIN customCharacters AS c ON s.characterID = c.characterID")))

(define (sql-super-update-affiliations lst)
  (let ([timestamp (srfi-date->sql-timestamp (current-date))])
    (for-each (lambda (x)
		(query sqlc "INSERT INTO customCharacters VALUES (?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE characterName=?,corporationID=?,corporationName=?,allianceID=?,allianceName=?,datetime=?"
		       (first x)
		       (second x)
		       (third x)
		       (fourth x)
		       (fifth x)
		       (sixth x)
		       timestamp
		       (second x)
		       (third x)
		       (fourth x)
		       (fifth x)
		       (sixth x)
		       timestamp))
	      lst)))

(define (sql-super-get-characterids)
  (query-list sqlc "SELECT characterID FROM intelSuperAffiliations"))

(define (sql-super-latest-killid)
  (query-value sqlc "SELECT MAX(killID) FROM intelSuperRaw"))

(define (sql-super-latest-datetime)
  (query-value sqlc "SELECT MAX(datetime) FROM intelSuperRaw"))

(define (sql-super-populate-affiliations)
  (query-exec sqlc "INSERT IGNORE INTO customCharacters ( characterID,characterName,corporationID,corporationName,allianceID,allianceName,datetime ) SELECT sub.*,0,'',0,'','0000-00-00 00:00:00' FROM ( SELECT DISTINCT characterID,characterName FROM intelSuperRaw ) AS sub"))

(define (sql-super-create-raw)
  (if (table-exists? sqlc "intelSuperRaw")
      #t
      (query-exec sqlc "CREATE TABLE intelSuperRaw ( shipTypeID INT NOT NULL, characterID INT NOT NULL, characterName VARCHAR(255) NOT NULL, corporationID INT NOT NULL, corporationName VARCHAR(255) NOT NULL, allianceID INT, allianceName VARCHAR(255), eventType VARCHAR(255), killID INT, victimTypeID INT, moonID INT, systemID INT, regionID INT, datetime DATETIME, UNIQUE KEY (characterID, killID) )")))

(define (sql-super-insert-killmails lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT IGNORE INTO intelSuperRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
		     (sql-killmail-shiptype x)
		     (sql-killmail-characterid x)
		     (sql-killmail-charactername x)
		     (sql-killmail-corporationid x)
		     (sql-killmail-corporationname x)
		     (sql-killmail-allianceid x)
		     (sql-killmail-alliancename x)
		     (sql-killmail-eventtype x)
		     (sql-killmail-killid x)
		     (sql-killmail-victimtype x)
		     (sql-killmail-location x)
		     (sql-killmail-system x)
		     (sql-killmail-region x)
		     (sql-killmail-datetime x)))
	    lst))

(define (sql-super-create-view)
  (if (table-exists? sqlc "intelSuperView")
      #t
      (query-exec sqlc (string-append "CREATE VIEW intelSuperView AS "
				      "SELECT "
				      "shipTypes.typeName AS shipTypeName,characterID,characterName,corporationID,"
				      "corporationName,allianceID,allianceName,eventType,"
				      "killID,victimTypes.typeName AS victimTypeName,mapSolarSystems.solarSystemName,"
				      "mapRegions.regionName,datetime "
				      "FROM intelSuperRaw "
				      "LEFT JOIN invTypes AS shipTypes ON shipTypes.typeID = intelSuperRaw.shipTypeID "
				      "LEFT JOIN invTypes AS victimTypes ON victimTypes.typeID = intelSuperRaw.victimTypeID "
				      "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = intelSuperRaw.systemID "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = intelSuperRaw.regionID "))))

(define (sql-super-create-latest)
  (if (table-exists? sqlc "intelSuperLatest")
      #t
      (query-exec sqlc (string-append "CREATE VIEW intelSuperLatest AS "
				      "SELECT "
				      "shipTypeID,characterID,characterName,corporationID,corporationName,allianceID,allianceName,"
				      "eventType,killID,victimTypeID,systemID,regionID,datetime,COUNT(killID) AS killCount,"
				      "SEC_TO_TIME(ROUND(AVG(TIME_TO_SEC(time(datetime))))) AS activityAvg,"
				      "ROUND((STDDEV_SAMP(TIME_TO_SEC(time(datetime))) / 3600),1) AS activityStd "
				      "FROM intelSuperRaw "
				      "GROUP BY characterID DESC "
				      "ORDER BY characterName"))))

(define (sql-super-create-watchlist)
  (if (table-exists? sqlc "intelSuperWatchlist")
      #t
      (query-exec sqlc (string-append "CREATE VIEW intelSuperWatchlist AS "
				      "SELECT "
				      "shipTypes.typeName AS shipTypeName,api.characterID,api.characterName,"
				      "api.corporationID,api.corporationName,api.allianceID,api.allianceName,"
				      "eventType,killID,victimTypes.typeName AS victimTypeName,mapSolarSystems.solarSystemName,"
				      "mapRegions.regionName,datetime,killCount,activityAvg,activityStd "
				      "FROM intelSuperLatest as latest "
				      "LEFT JOIN invTypes AS shipTypes ON shipTypes.typeID = latest.shipTypeID "
				      "LEFT JOIN invTypes AS victimTypes ON victimTypes.typeID = latest.victimTypeID "
				      "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = latest.systemID "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = latest.regionID "
				      "LEFT JOIN intelSuperAffiliations AS api ON api.characterID = latest.characterID "
				      "ORDER BY latest.datetime"))))

;; Backwards compatibility

(define (super-replace-killmails lst) (sql-super-insert-killmails lst))

(define (sql-filter-watchlist)
  (query-rows sqlc "SELECT characterName,shipTypeName,allianceName FROM intelSuperWatchlist"))
