#lang racket

(require db)

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")

(provide (all-defined-out))

(define (sql-citadel-create-raw)
  (if (table-exists? sqlc "citadelScanRaw")
      #t
      (query-exec sqlc "CREATE TABLE citadelScanRaw ( regionID INT NOT NULL, constellationID INT NOT NULL, solarSystemID INT NOT NULL, locationID INT NOT NULL, allianceTicker VARCHAR(10), corporationTicker VARCHAR(10), datetime DATETIME, typeID INT, scanID VARCHAR(64), UNIQUE KEY ( scanID ) )")))

(define (sql-citadel-update-scan lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO citadelScanRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE allianceTicker=?,corporationTicker=?,datetime=?,typeID=?,scanID=?"
		     (sql-citadel-region x)
		     (sql-citadel-constellation x)
		     (sql-citadel-system x)
		     (sql-citadel-location x)
		     (sql-citadel-alliance x)
		     (sql-citadel-corporation x)
		     (sql-citadel-datetime x)
		     (sql-citadel-type x)
		     (sql-citadel-scanid x)
		     (sql-citadel-alliance x)
		     (sql-citadel-corporation x)
		     (sql-citadel-datetime x)
		     (sql-citadel-type x)
		     (sql-citadel-scanid x)))
	    lst))

(define (sql-citadel-create-id-table)
  (if (table-exists? sqlc "citadelScanID")
      #t
      (query-exec sqlc "CREATE TABLE citadelScanID ( citadelID VARCHAR(64) NOT NULL, scanID VARCHAR(64) NOT NULL, UNIQUE KEY ( scanID ) )")))

(define (sql-citadel-create-id-procedure)
  (query-exec sqlc "CREATE PROCEDURE generateCitadelID () READS SQL DATA NOT DETERMINISTIC INSERT IGNORE INTO citadelScanID SELECT CONCAT_WS('-',base,derivedID) AS citadelID,scanID FROM (SELECT CONCAT_WS('-',locationID,corp.corporationID,typeID) AS base,scanID,@id := if(@data = CONCAT_WS('-',locationID,corp.corporationTicker,typeID), @id + 1, 1) AS derivedID,@data := CONCAT_WS('-',locationID,corp.corporationTicker,typeID) AS dummy FROM citadelScanRaw AS scan LEFT JOIN customCorporations AS corp ON corp.corporationTicker =  scan.corporationTicker ORDER BY CONCAT_WS('-',locationID,corp.corporationID,typeID) ) AS sub"))

(define (sql-citadel-delete-id-procedure)
  (query-exec sqlc "DROP PROCEDURE IF EXISTS generateCitadelID"))

(define (sql-citadel-create-pseudomaterialized-view)
  (if (table-exists? sqlc "citadelScanMV")
      #t
      (query-exec sqlc (string-append "CREATE TABLE citadelScanMV "
				      "( UNIQUE KEY (scanID) ) "
				      "AS SELECT "
				      "mapRegions.regionName,mapConstellations.constellationName,"
				      "mapSolarSystems.solarSystemName,mapDenormalize.itemName AS locationName,"
				      "scan.allianceTicker,customAlliances.allianceName,scan.corporationTicker,"
				      "customCorporations.corporationName,scan.datetime,invTypes.typeName,"
				      "IF(citadelKillRaw.datetime > scan.datetime, 'RESCAN', 'SCANNED') AS 'checkStatus', "
				      "scan.scanID,citadelScanID.citadelID "
				      "FROM citadelScanRaw AS scan "
				      "LEFT JOIN mapRegions ON mapRegions.regionID = scan.regionID "
				      "LEFT JOIN mapConstellations ON mapConstellations.constellationID = scan.constellationID "
				      "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = scan.solarSystemID "
				      "LEFT JOIN mapDenormalize ON mapDenormalize.itemID = scan.locationID "
				      "LEFT JOIN invTypes ON invTypes.typeID = scan.typeID "
				      "LEFT JOIN customAlliances ON customAlliances.allianceTicker = scan.allianceTicker "
				      "LEFT JOIN customCorporations ON customCorporations.corporationTicker = scan.corporationTicker "
				      "LEFT JOIN citadelScanID ON citadelScanID.scanID = scan.scanID "
				      "LEFT JOIN citadelKillRaw ON citadelKillRaw.solarSystemID = scan.solarSystemID "
				      "AND citadelKillRaw.typeID = scan.typeID"))))

(define (sql-citadel-create-view)
  (if (table-exists? sqlc "citadelScanView")
      #t
      (query-exec sqlc "CREATE VIEW citadelScanView AS SELECT regionName,constellationName,solarSystemName,locationName,allianceTicker,allianceName,corporationTicker,corporationName,datetime,typeName,checkStatus,scanID,citadelID FROM citadelScanMV")))

(define (sql-citadel-create-kill-raw)
  (if (table-exists? sqlc "citadelKillRaw")
      #t
      (query-exec sqlc "CREATE TABLE citadelKillRaw ( regionID INT NOT NULL, solarSystemID INT NOT NULL, locationID INT, corporationID INT NOT NULL, allianceID INT, datetime DATETIME NOT NULL, typeID INT, killID INT, UNIQUE KEY (killID) )")))

;; Triggers for citadelScanRaw

(define (sql-citadel-create-trigger-insert)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER insert_citadelScanRaw AFTER INSERT ON citadelScanRaw "
		    "FOR EACH ROW BEGIN "
		    "SET @id = 0; "
		    "REPLACE INTO citadelScanID SELECT CONCAT_WS('-',base,derivedID) AS citadelID,scanID "
		    "FROM (SELECT CONCAT_WS('-',locationID,corp.corporationID,typeID) AS base,scanID,"
		    "@id := if(@data = CONCAT_WS('-',locationID,corp.corporationTicker,typeID), @id + 1, 1) AS derivedID,"
		    "@data := CONCAT_WS('-',locationID,corp.corporationTicker,typeID) AS dummy "
		    "FROM citadelScanRaw AS scan "
		    "LEFT JOIN customCorporations AS corp ON corp.corporationTicker=scan.corporationTicker "
		    "ORDER BY CONCAT_WS('-',locationID,corp.corporationID,typeID) ) AS sub; "
		    "INSERT INTO citadelScanMV "
		    "SELECT "
		    "mapRegions.regionName,mapConstellations.constellationName,"
		    "mapSolarSystems.solarSystemName,mapDenormalize.itemName AS locationName,"
		    "NEW.allianceTicker,customAlliances.allianceName,NEW.corporationTicker,"
		    "customCorporations.corporationName,NEW.datetime,invTypes.typeName,"
		    "IF(citadelKillRaw.datetime > NEW.datetime, 'RESCAN', 'SCANNED') AS 'checkStatus', "
		    "NEW.scanID,citadelScanID.citadelID "
		    "FROM citadelScanRaw "
		    "LEFT JOIN mapRegions ON mapRegions.regionID = NEW.regionID "
		    "LEFT JOIN mapConstellations ON mapConstellations.constellationID = NEW.constellationID "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = NEW.solarSystemID "
		    "LEFT JOIN mapDenormalize ON mapDenormalize.itemID = NEW.locationID "
		    "LEFT JOIN invTypes ON invTypes.typeID = NEW.typeID "
		    "LEFT JOIN customAlliances ON customAlliances.allianceTicker = NEW.allianceTicker "
		    "LEFT JOIN customCorporations ON customCorporations.corporationTicker = NEW.corporationTicker "
		    "LEFT JOIN citadelScanID ON citadelScanID.scanID = NEW.scanID "
		    "LEFT JOIN citadelKillRaw ON citadelKillRaw.solarSystemID = NEW.solarSystemID "
		    "AND citadelKillRaw.typeID = NEW.typeID "
		    "WHERE citadelScanRaw.scanID=NEW.scanID; "
		    "END;")))

(define (sql-citadel-create-trigger-delete)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER delete_citadelScanRaw AFTER DELETE ON citadelScanRaw "
		    "FOR EACH ROW BEGIN "
		    "DELETE FROM citadelScanID "
		    "WHERE citadelScanID.scanID=OLD.scanID; "
		    "DELETE FROM citadelScanMV "
		    "WHERE citadelScanMV.scanID=OLD.scanID; "
		    "END;")))

(define (sql-citadel-create-trigger-update)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER update_citadelScanRaw AFTER UPDATE ON citadelScanRaw "
		    "FOR EACH ROW BEGIN "
		    "SET @id = 0; "
		    "REPLACE INTO citadelScanID SELECT CONCAT_WS('-',base,derivedID) AS citadelID,scanID "
		    "FROM (SELECT CONCAT_WS('-',locationID,corp.corporationID,typeID) AS base,scanID,"
		    "@id := if(@data = CONCAT_WS('-',locationID,corp.corporationTicker,typeID), @id + 1, 1) AS derivedID,"
		    "@data := CONCAT_WS('-',locationID,corp.corporationTicker,typeID) AS dummy "
		    "FROM citadelScanRaw AS scan "
		    "LEFT JOIN customCorporations AS corp ON corp.corporationTicker=scan.corporationTicker "
		    "ORDER BY CONCAT_WS('-',locationID,corp.corporationID,typeID) ) AS sub; "
		    "UPDATE citadelScanMV AS mv "
		    "LEFT JOIN customAlliances ON customAlliances.allianceTicker = NEW.allianceTicker "
		    "LEFT JOIN customCorporations ON customCorporations.corporationTicker = NEW.corporationTicker "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemID = NEW.solarSystemID "
		    "LEFT JOIN invTypes ON invTypes.typeID = NEW.typeID "
		    "LEFT JOIN citadelScanID ON citadelScanID.scanID = NEW.scanID "
		    "LEFT JOIN citadelKillRaw ON citadelKillRaw.solarSystemID = NEW.solarSystemID "
		    "AND citadelKillRaw.typeID = NEW.typeID "
		    "SET "
		    "mv.allianceTicker=NEW.allianceTicker,"
		    "mv.allianceName=customAlliances.allianceName,"
		    "mv.corporationTicker=NEW.corporationTicker,"
		    "mv.corporationName=customCorporations.corporationName,"
		    "mv.datetime=NEW.datetime,"
		    "mv.typeName=invTypes.typeName,"
		    "mv.checkStatus=IF(citadelKillRaw.datetime > NEW.datetime, 'RESCAN', 'SCANNED'), "
		    "mv.scanID=NEW.scanID, "
		    "mv.citadelID=citadelScanID.citadelID "
		    "WHERE mv.scanID=OLD.scanID; "
		    "END;")))

(define (sql-citadel-create-triggers)
  (begin
    (query-exec sqlc "DROP TRIGGER IF EXISTS insert_citadelScanRaw")
    (sql-citadel-create-trigger-insert)
    (query-exec sqlc "DROP TRIGGER IF EXISTS delete_citadelScanRaw")
    (sql-citadel-create-trigger-delete)
    (query-exec sqlc "DROP TRIGGER IF EXISTS update_citadelScanRaw")
    (sql-citadel-create-trigger-update)))

;; Triggers for citadelKillRaw

(define (sql-citadel-create-kill-trigger-insert)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER insert_citadelKillRaw AFTER INSERT ON citadelKillRaw "
		    "FOR EACH ROW BEGIN "
		    "UPDATE citadelScanMV AS mv "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemName = mv.solarSystemName "
		    "LEFT JOIN invTypes ON invTypes.typeName = mv.typeName "
		    "SET "
		    "mv.checkStatus=IF(NEW.datetime > mv.datetime, 'RESCAN', 'SCANNED')"
		    "WHERE mapSolarSystems.solarSystemID=NEW.solarSystemID "
		    "AND invTypes.typeID=NEW.typeID; "
		    "END;")))

(define (sql-citadel-create-kill-trigger-delete)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER delete_citadelKillRaw AFTER DELETE ON citadelKillRaw "
		    "FOR EACH ROW BEGIN "
		    "UPDATE citadelScanMV AS mv "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemName = mv.solarSystemName "
		    "LEFT JOIN invTypes ON invTypes.typeName = mv.typeName "
		    "SET "
		    "mv.checkStatus='SCANNED' "
		    "WHERE mapSolarSystems.solarSystemID=OLD.solarSystemID "
		    "AND invTypes.typeID=OLD.typeID; "
		    "END;")))

(define (sql-citadel-create-kill-trigger-update)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER update_citadelKillRaw AFTER UPDATE ON citadelKillRaw "
		    "FOR EACH ROW BEGIN "
		    "UPDATE citadelScanMV AS mv "
		    "LEFT JOIN mapSolarSystems ON mapSolarSystems.solarSystemName = mv.solarSystemName "
		    "LEFT JOIN invTypes ON invTypes.typeName = mv.typeName "
		    "SET "
		    "mv.checkStatus=IF(NEW.datetime > mv.datetime, 'RESCAN', 'SCANNED')"
		    "WHERE mapSolarSystems.solarSystemID=NEW.solarSystemID "
		    "AND invTypes.typeID=NEW.typeID; "
		    "END;")))

(define (sql-citadel-create-kill-triggers)
  (begin
    (query-exec sqlc "DROP TRIGGER IF EXISTS insert_citadelKillRaw")
    (sql-citadel-create-kill-trigger-insert)
    (query-exec sqlc "DROP TRIGGER IF EXISTS delete_citadelKillRaw")
    (sql-citadel-create-kill-trigger-delete)
    (query-exec sqlc "DROP TRIGGER IF EXISTS update_citadelKillRaw")
    (sql-citadel-create-kill-trigger-update)))
