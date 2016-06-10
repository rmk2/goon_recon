#lang racket

(require db)

(require "eve-sql_main.rkt")
(require "eve-string_tools.rkt")

(provide (all-defined-out))

(define (parse-moondata lst)
  (query-maybe-row sqlc (string-append
			 "SELECT corporationTicker,allianceTicker,datetime,moonType "
			 "FROM moondata WHERE solarSystemName = ? AND planet = ? AND moon = ?")
		   (first lst)
		   (second lst)
		   (third lst)))

(define-syntax money-moon?
  (syntax-rules ()
    ((_ arg) (cond
	      [(list? arg) (if (parse-moondata arg) #t #f)]
	      [(string? arg) (if (parse-moondata (split-moon-display arg)) #t #f)]
	      [else #f]))))

(define (sql-moon-create-raw)
  (if (table-exists? sqlc "moonScanRaw")
      #t
      (query-exec sqlc "CREATE TABLE moonScanRaw ( regionID INT NOT NULL, constellationID INT NOT NULL, solarSystemID INT NOT NULL, planet INT NOT NULL, moon INT NOT NULL, allianceTicker VARCHAR(10), corporationTicker VARCHAR(10), datetime DATETIME, typeID INT, online TINYINT(1), UNIQUE KEY (solarSystemID, planet, moon) )")))

(define (sql-moon-update-scan lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO moonScanRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE allianceTicker=?,corporationTicker=?,datetime=?,typeID=?,online=?"
		     (first x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)
		     (seventh x)
		     (eighth x)
		     (ninth x)
		     (tenth x)
		     (sixth x)
		     (seventh x)
		     (eighth x)
		     (ninth x)
		     (tenth x)))
	    lst))

(define (sql-moon-create-pseudomaterialized-view)
  (if (table-exists? sqlc "moonScanMV")
      #t
      (query-exec sqlc (string-append "CREATE TABLE moonScanMV AS "
				      "SELECT "
				      "mapRegions.regionName,mapConstellations.constellationName,"
				      "mapSolarSystems.solarSystemName,scan.planet,scan.moon,scan.allianceTicker,"
				      "customAlliances.allianceName,scan.corporationTicker,customCorporations.corporationName,"
				      "scan.datetime,invTypes.typeName,data.moonType,"
				      "IF(scan.online = 1, 'ONLINE', 'OFFLINE') AS 'online',"
				      "IF(towerKillRaw.datetime > scan.datetime, 'RESCAN', 'SCANNED') AS 'checkStatus' "
				      "FROM moonScanRaw AS scan "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = scan.regionID "
				      "LEFT JOIN mapConstellations ON mapConstellations.constellationID = scan.constellationID "
				      "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = scan.solarSystemID "
				      "LEFT JOIN invTypes ON invTypes.typeID = scan.typeID "
				      "LEFT JOIN customAlliances ON customAlliances.allianceTicker = scan.allianceTicker "
				      "LEFT JOIN customCorporations ON customCorporations.corporationTicker = scan.corporationTicker "
				      "LEFT JOIN moondata AS data ON data.solarSystemName = mapSolarSystems.solarSystemName "
				      "AND data.planet = scan.planet "
				      "AND data.moon = scan.moon "
				      "LEFT JOIN towerKillRaw ON scan.solarSystemID = towerKillRaw.solarSystemID "
				      "AND scan.planet = towerKillRaw.planet "
				      "AND scan.moon = towerKillRaw.moon"))))

(define (sql-moon-create-view)
  (if (table-exists? sqlc "moonScanVIEW")
      #t
      (query-exec sqlc "CREATE VIEW moonScanView AS SELECT regionName,constellationName,solarsystemName,planet,moon,allianceTicker,allianceName,corporationTicker,corporationName,datetime,typeName,moonType,online,checkStatus FROM moonScanMV")))

(define (sql-moon-region-towers param)
  (map vector->list (query-rows sqlc "SELECT regionName,constellationName,solarsystemName,planet,moon,allianceTicker,allianceName,corporationTicker,corporationName,datetime,typeName,moonType,online,checkStatus FROM moonScanView WHERE regionName LIKE ?" param)))

(define (sql-moon-get-towers)
  (query-rows sqlc "SELECT regionName,constellationName,solarsystemName,planet,moon,allianceTicker,allianceName,corporationTicker,corporationName,datetime,typeName,moonType,online,checkStatus FROM moonScanView"))

(define (sql-moon-create-tasks)
  (if (table-exists? sqlc "moonScanTasks")
      #t
      (query-exec sqlc "CREATE VIEW moonScanTasks AS SELECT regionName,constellationName,solarSystemName,planet,moon,allianceTicker,corporationTicker,datetime,typeName FROM moonScanMV WHERE checkStatus = 'RESCAN'")))

(define (sql-moon-get-tasks)
  (query-rows sqlc "SELECT regionName,constellationName,solarSystemName,planet,moon,allianceTicker,corporationTicker,datetime,typeName FROM moonScanTasks"))

(define (sql-moon-region-tasks param)
  (map vector->list (query-rows sqlc "SELECT regionName,constellationName,solarSystemName,planet,moon,allianceTicker,corporationTicker,datetime,typeName FROM moonScanTasks WHERE regionName LIKE ?" param)))

(define (sql-goo-create-guess)
  (if (table-exists? sqlc "moonGooGuess")
      #t
      (query-exec sqlc "CREATE TABLE moonGooGuess (regionID INT NOT NULL, constellationID INT NOT NULL, solarSystemID INT NOT NULL, planet INT NOT NULL, moon INT NOT NULL,moonType INT, UNIQUE KEY (solarSystemID, planet, moon, moonType) )")))

(define (sql-goo-create-raw)
  (if (table-exists? sqlc "moonGooRaw")
      #t
      (query-exec sqlc "CREATE TABLE moonGooRaw (regionID INT NOT NULL, constellationID INT NOT NULL, solarSystemID INT NOT NULL, planet INT NOT NULL, moon INT NOT NULL,moonType INT, UNIQUE KEY (solarSystemID, planet, moon, moonType) )")))

;; Define triggers to update the pseudo-materialized table we created above
;; whenever moonScanRaw changes, since a simple view on its own is too slow.

(define (sql-moon-create-trigger-insert)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER insert_moonScanMV AFTER INSERT ON moonScanRaw "
		    "FOR EACH ROW BEGIN "
		    "INSERT INTO moonScanMV "
		    "SELECT "
		    "mapRegions.regionName,mapConstellations.constellationName,"
		    "mapSolarSystems.solarSystemName,NEW.planet,NEW.moon,NEW.allianceTicker,"
		    "customAlliances.allianceName,NEW.corporationTicker,customCorporations.corporationName,"
		    "NEW.datetime,invTypes.typeName,data.moonType,"
		    "IF(NEW.online = 1, 'ONLINE', 'OFFLINE') AS 'online',"
		    "IF(towerKillRaw.datetime > NEW.datetime, 'RESCAN', 'SCANNED') AS 'checkStatus' "
		    "FROM moonScanRaw "
		    "LEFT JOIN mapRegions ON mapRegions.regionID = NEW.regionID "
		    "LEFT JOIN mapConstellations ON mapConstellations.constellationID = NEW.constellationID "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = NEW.solarSystemID "
		    "LEFT JOIN invTypes ON invTypes.typeID = NEW.typeID "
		    "LEFT JOIN customAlliances ON customAlliances.allianceTicker = NEW.allianceTicker "
		    "LEFT JOIN customCorporations ON customCorporations.corporationTicker = NEW.corporationTicker "
		    "LEFT JOIN moondata AS data ON data.solarSystemName = mapSolarSystems.solarSystemName "
		    "AND data.planet = NEW.planet "
		    "AND data.moon = NEW.moon "
		    "LEFT JOIN towerKillRaw ON NEW.solarSystemID = towerKillRaw.solarSystemID "
		    "AND NEW.planet = towerKillRaw.planet "
		    "AND NEW.moon = towerKillRaw.moon "
		    "WHERE moonScanRaw.solarSystemID=NEW.solarSystemID "
		    "AND moonScanRaw.planet=NEW.planet "
		    "AND moonScanRaw.moon=NEW.moon; "
		    "END;")))

(define (sql-moon-create-trigger-delete)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER delete_moonScanMV AFTER DELETE ON moonScanRaw "
		    "FOR EACH ROW BEGIN "
		    "DELETE moonScanMV FROM moonScanMV "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = OLD.solarSystemID "
		    "WHERE mapSolarSystems.solarSystemID=OLD.solarSystemID AND planet=OLD.planet AND moon=OLD.moon; "
		    "END;")))

(define (sql-moon-create-trigger-update)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER update_moonScanMV AFTER UPDATE ON moonScanRaw "
		    "FOR EACH ROW BEGIN "
		    "UPDATE moonScanMV AS mv "
		    "LEFT JOIN customAlliances ON customAlliances.allianceTicker = NEW.allianceTicker "
		    "LEFT JOIN customCorporations ON customCorporations.corporationTicker = NEW.corporationTicker "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = NEW.solarSystemID "
		    "LEFT JOIN invTypes ON invTypes.typeID = NEW.typeID "
		    "SET "
		    "mv.allianceTicker=NEW.allianceTicker,"
		    "mv.allianceName=customAlliances.allianceName,"
		    "mv.corporationTicker=NEW.corporationTicker,"
		    "mv.corporationName=customCorporations.corporationName,"
		    "mv.datetime=NEW.datetime,"
		    "mv.typeName=invTypes.typeName,"
		    "mv.online=IF(NEW.online = 1, 'ONLINE', 'OFFLINE') "
		    "WHERE mapSolarSystems.solarSystemID=OLD.solarSystemID AND mv.planet=OLD.planet AND mv.moon=OLD.moon; "
		    "END;")))

(define (sql-moon-create-triggers)
  (begin
    (query-exec sqlc "DROP TRIGGER IF EXISTS insert_moonScanMV")
    (sql-moon-create-trigger-insert)
    (query-exec sqlc "DROP TRIGGER IF EXISTS delete_moonScanMV")
    (sql-moon-create-trigger-delete)
    (query-exec sqlc "DROP TRIGGER IF EXISTS update_moonScanMV")
    (sql-moon-create-trigger-update)))
