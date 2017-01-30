#lang racket

(require db)
(require db/util/datetime)
(require srfi/19)

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
  (query-exec sqlc "INSERT IGNORE INTO customCharacters ( characterID,characterName,corporationID,corporationName,allianceID,allianceName,datetime ) SELECT sub.*,0,'',0,'','0000-01-01 00:00:00' FROM ( SELECT DISTINCT characterID,characterName FROM intelSuperRaw ) AS sub"))

(define (sql-super-create-raw)
  (if (table-exists? sqlc "intelSuperRaw")
      #t
      (query-exec sqlc "CREATE TABLE intelSuperRaw ( shipTypeID INT NOT NULL, characterID INT NOT NULL, characterName VARCHAR(255) NOT NULL, corporationID INT NOT NULL, corporationName VARCHAR(255) NOT NULL, allianceID INT, allianceName VARCHAR(255), eventType VARCHAR(255), killID INT, victimTypeID INT, moonID INT, solarSystemID INT, regionID INT, datetime DATETIME, UNIQUE KEY (characterID, killID), KEY (datetime), KEY (eventType) )")))

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
				      "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = intelSuperRaw.solarSystemID "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = intelSuperRaw.regionID "))))

(define (sql-super-create-latest-pseudomaterialized-view)
  (if (table-exists? sqlc "intelSuperLatestMV")
      #t
      (query-exec sqlc (string-append
			"CREATE TABLE intelSuperLatestMV "
			"( UNIQUE KEY (characterID), KEY (corporationID), KEY (allianceID), "
			"KEY (eventType), KEY (killCount), KEY (activityAvg), KEY (activityStd), KEY (datetime) ) "
			"AS SELECT "
			"ship.TypeID AS shipTypeID,ship.typeName AS shipTypeName,characterID,characterName,"
			"corporation.corporationID,corporation.corporationTicker,corporation.corporationName,"
			"alliance.allianceID,alliance.allianceTicker,alliance.allianceName,eventType,killID,"
			"victim.TypeID AS victimTypeID,victim.TypeName AS victimTypeName,"
			"solar.solarSystemID,solar.solarSystemName,constellation.constellationID,"
			"constellation.constellationName,region.regionID,region.regionName,datetime,COUNT(killID) AS killCount,"
			"SEC_TO_TIME(ROUND(AVG(TIME_TO_SEC(time(datetime))))) AS activityAvg,"
			"ROUND((STDDEV_SAMP(TIME_TO_SEC(time(datetime))) / 3600),1) AS activityStd "
			"FROM intelSuperRaw AS mv "
			"LEFT JOIN customAlliances AS alliance ON alliance.allianceID = mv.allianceID "
			"LEFT JOIN customCorporations AS corporation ON corporation.corporationID = mv.corporationID "
			"LEFT JOIN mapSolarSystems AS solar ON solar.solarSystemID = mv.solarSystemID "
			"LEFT JOIN mapConstellations AS constellation ON constellation.constellationID = solar.constellationID "
			"LEFT JOIN mapRegions AS region ON region.regionID = mv.regionID "
			"LEFT JOIN invTypes AS ship ON ship.typeID = mv.shipTypeID "
			"LEFT JOIN invTypes AS victim ON victim.typeID = mv.victimTypeID "
			"GROUP BY characterID DESC"))))

(define (sql-super-create-watchlist)
  (if (table-exists? sqlc "intelSuperWatchlist")
      #t
      (query-exec sqlc (string-append
			"CREATE VIEW intelSuperWatchlist AS "
			"SELECT "
			"shipTypeID,shipTypeName,api.characterID,api.characterName,"
			"corporation.corporationID,corporation.corporationTicker,corporation.corporationName,"
			"alliance.allianceID,alliance.allianceTicker,alliance.allianceName,eventType,killID,"
			"victimTypeID,victimTypeName,solarSystemID,solarSystemName,constellationID,"
			"constellationName,regionID,regionName,datetime,killCount,activityAvg,activityStd "
			"FROM intelSuperLatestMV as mv "
			"INNER JOIN intelSuperAffiliations AS api ON api.characterID = mv.characterID "
			"LEFT JOIN customAlliances AS alliance ON alliance.allianceID = api.allianceID "
			"LEFT JOIN customCorporations AS corporation ON corporation.corporationID = api.corporationID "))))

;; Backwards compatibility

(define (super-replace-killmails lst) (sql-super-insert-killmails lst))

(define (sql-filter-watchlist)
  (query-rows sqlc "SELECT characterName,shipTypeName,allianceName FROM intelSuperWatchlist"))

;; Update triggers for intelSuperLatestMV (pseudomaterialized view)

(define (sql-super-latest-create-trigger-insert)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER insert_intelSuperLatestMV AFTER INSERT ON intelSuperRaw "
		    "FOR EACH ROW BEGIN "
		    "REPLACE INTO intelSuperLatestMV "
		    "SELECT "
		    "ship.TypeID,ship.TypeName,characterID,characterName,corporation.corporationID,corporation.corporationTicker,"
		    "corporation.corporationName,alliance.allianceID,alliance.allianceTicker,alliance.allianceName,"
		    "eventType,killID,victim.TypeID,victim.TypeName,solar.solarSystemID,solar.solarSystemName,"
		    "constellation.constellationID,constellation.constellationName,region.regionID,region.regionName,datetime,"
		    "COUNT(killID) AS killCount,"
		    "SEC_TO_TIME(ROUND(AVG(TIME_TO_SEC(time(datetime))))) AS activityAvg,"
		    "ROUND((STDDEV_SAMP(TIME_TO_SEC(time(datetime))) / 3600),1) AS activityStd "
		    "FROM intelSuperRaw "
		    "LEFT JOIN customAlliances AS alliance ON alliance.allianceID = NEW.allianceID "
		    "LEFT JOIN customCorporations AS corporation ON corporation.corporationID = NEW.corporationID "
		    "LEFT JOIN mapSolarSystems AS solar ON solar.solarSystemID = NEW.solarSystemID "
		    "LEFT JOIN mapConstellations AS constellation ON constellation.constellationID = solar.constellationID "
		    "LEFT JOIN mapRegions AS region ON region.regionID = NEW.regionID "
		    "LEFT JOIN invTypes AS ship ON ship.typeID = NEW.shipTypeID "
		    "LEFT JOIN invTypes AS victim ON victim.typeID = NEW.victimTypeID "
		    "WHERE characterID = NEW.characterID "
		    "GROUP BY characterID DESC; "
		    "END;")))

(define (sql-super-latest-create-trigger-delete)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER delete_intelSuperLatestMV AFTER DELETE ON intelSuperRaw "
		    "FOR EACH ROW BEGIN "
		    "DELETE FROM intelSuperLatestMV "
		    "WHERE intelSuperLatestMV.characterID = OLD.characterID; "
		    "END;")))

(define (sql-super-latest-create-triggers)
  (begin
    (query-exec sqlc "DROP TRIGGER IF EXISTS insert_intelSuperLatestMV")
    (sql-super-latest-create-trigger-insert)
    (query-exec sqlc "DROP TRIGGER IF EXISTS delete_intelSuperLatestMV")
    (sql-super-latest-create-trigger-delete)))
