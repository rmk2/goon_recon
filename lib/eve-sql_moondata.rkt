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

(define (sql-moon-create-view)
  (if (table-exists? sqlc "moonScanView")
      #t
      (query-exec sqlc (string-append "CREATE VIEW moonScanView AS "
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

(define (sql-moon-region-towers param)
  (map vector->list (query-rows sqlc "SELECT regionName,constellationName,solarsystemName,planet,moon,allianceTicker,allianceName,corporationTicker,corporationName,datetime,typeName,moonType,online,checkStatus FROM moonScanView WHERE regionName LIKE ?" param)))

(define (sql-moon-get-towers)
  (query-rows sqlc "SELECT regionName,constellationName,solarsystemName,planet,moon,allianceTicker,allianceName,corporationTicker,corporationName,datetime,typeName,moonType,online,checkStatus FROM moonScanView"))

(define (sql-goo-create-guess)
  (if (table-exists? sqlc "moonGooGuess")
      #t
      (query sqlc "CREATE TABLE moonGooGuess (regionID INT NOT NULL, constellationID INT NOT NULL, solarSystemID INT NOT NULL, planet INT NOT NULL, moon INT NOT NULL,moonType INT, UNIQUE KEY (solarSystemID, planet, moon, moonType) )")))

(define (sql-goo-create-raw)
  (if (table-exists? sqlc "moonGooRaw")
      #t
      (query sqlc "CREATE TABLE moonGooRaw (regionID INT NOT NULL, constellationID INT NOT NULL, solarSystemID INT NOT NULL, planet INT NOT NULL, moon INT NOT NULL,moonType INT, UNIQUE KEY (solarSystemID, planet, moon, moonType) )")))
