#lang racket

(require db)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define (sql-tower-create-raw)
  (if (table-exists? sqlc "towerKillRaw")
      #t
      (query-exec sqlc "CREATE TABLE towerKillRaw ( regionID INT NOT NULL, solarSystemID INT NOT NULL, planet INT NOT NULL, moon INT NOT NULL, corporationID INT NOT NULL, allianceID INT, datetime DATETIME NOT NULL, typeID INT, killID INT, UNIQUE KEY (solarSystemID, planet, moon) )")))

(define (sql-tower-update-raw lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO towerKillRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE corporationID=?,allianceID=?,datetime=?,typeID=?,killID=?"
		     (first x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)
		     (seventh x)
		     (eighth x)
		     (ninth x)
		     (fifth x)
		     (sixth x)
		     (seventh x)
		     (eighth x)
		     (ninth x)))
	    lst))

(define (sql-tower-create-view)
  (if (table-exists? sqlc "towerKillView")
      #t
      (query-exec sqlc (string-append "CREATE VIEW towerKillView AS "
				      "SELECT regionName,solarSystemName,planet,moon,corporationName,"
				      "allianceName,datetime,towerKillRaw.typeID,killID "
				      "FROM towerKillRaw "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = towerKillRaw.regionID "
				      "LEFT JOIN mapSolarSystems ON towerKillRaw.solarSystemID = mapSolarSystems.solarSystemID "
				      "LEFT JOIN customCorporations ON customCorporations.corporationID = towerKillRaw.corporationID "
				      "LEFT JOIN customAlliances ON customAlliances.allianceID = towerKillRaw.allianceID"))))

;; Define triggers to update the pseudo-materialized table we created for
;; moonScanRaw when towerKillRaw changes

(define (sql-tower-trigger-insert)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER insert_towerKillRaw AFTER INSERT ON towerKillRaw "
		    "FOR EACH ROW BEGIN "
		    "UPDATE moonScanMV AS mv "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemName = mv.solarSystemName "
		    "SET "
		    "mv.checkStatus=IF(NEW.datetime > mv.datetime, 'RESCAN', 'SCANNED')"
		    "WHERE mapSolarSystems.solarSystemID=NEW.solarSystemID AND mv.planet=NEW.planet AND mv.moon=NEW.moon; "
		    "END;")))

(define (sql-tower-trigger-update)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER update_towerKillRaw AFTER UPDATE ON towerKillRaw "
		    "FOR EACH ROW BEGIN "
		    "UPDATE moonScanMV AS mv "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemName = mv.solarSystemName "
		    "SET "
		    "mv.checkStatus=IF(NEW.datetime > mv.datetime, 'RESCAN', 'SCANNED')"
		    "WHERE mapSolarSystems.solarSystemID=NEW.solarSystemID AND mv.planet=NEW.planet AND mv.moon=NEW.moon; "
		    "END;")))

(define (sql-tower-trigger-delete)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER delete_towerKillRaw AFTER DELETE ON towerKillRaw "
		    "FOR EACH ROW BEGIN "
		    "UPDATE moonScanMV AS mv "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemName = mv.solarSystemName "
		    "SET "
		    "mv.checkStatus='SCANNED' "
		    "WHERE mapSolarSystems.solarSystemID=OLD.solarSystemID AND mv.planet=OLD.planet AND mv.moon=OLD.moon; "
		    "END;")))

(define (sql-tower-create-triggers)
  (begin
    (query-exec sqlc "DROP TRIGGER IF EXISTS insert_towerKillRaw")
    (sql-tower-trigger-insert)
    (query-exec sqlc "DROP TRIGGER IF EXISTS update_towerKillRaw")
    (sql-tower-trigger-update)
    (query-exec sqlc "DROP TRIGGER IF EXISTS delete_towerKillRaw")
    (sql-tower-trigger-delete)))
