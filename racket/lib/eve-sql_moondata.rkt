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
      (query-exec sqlc "CREATE TABLE moonScanRaw ( regionID INT NOT NULL, constellationID INT NOT NULL, solarSystemID INT NOT NULL, planet INT NOT NULL, moon INT NOT NULL, allianceTicker VARCHAR(10), corporationTicker VARCHAR(10), datetime DATETIME, typeID INT, UNIQUE KEY (solarSystemID, planet, moon) )")))

(define (sql-moon-update-scan lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO moonScanRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE allianceTicker=?,corporationTicker=?,datetime=?,typeID=?"
		     (first x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)
		     (seventh x)
		     (eighth x)
		     (ninth x)
		     (sixth x)
		     (seventh x)
		     (eighth x)
		     (ninth x)))
	    lst))

(define (sql-moon-create-view)
  (if (table-exists? sqlc "moonScanView")
      #t
      (query-exec sqlc (string-append "CREATE VIEW moonScanView AS "
				      "SELECT "
				      "mapRegions.regionName,mapConstellations.constellationName,"
				      "mapSolarSystems.solarSystemName,scan.planet,scan.moon,scan.allianceTicker,"
				      "customAlliances.allianceName,scan.corporationTicker,customCorporations.corporationName,"
				      "scan.datetime,invTypes.typeName,data.moonType "
				      "FROM moonScanRaw AS scan "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = scan.regionID "
				      "LEFT JOIN mapConstellations ON mapConstellations.constellationID = scan.constellationID "
				      "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = scan.solarSystemID "
				      "LEFT JOIN invTypes ON invTypes.typeID = scan.typeID "
				      "LEFT JOIN customAlliances ON customAlliances.allianceTicker = scan.allianceTicker "
				      "LEFT JOIN customCorporations ON customCorporations.corporationTicker = scan.corporationTicker "
				      "LEFT JOIN moondata AS data ON data.solarSystemName = mapSolarSystems.solarSystemName "
				      "AND data.planet = scan.planet "
				      "AND data.moon = scan.moon"))))
