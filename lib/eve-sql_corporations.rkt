#lang racket

(require db)

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")

(provide (all-defined-out))

(define (sql-corporation-create-raw)
  (if (table-exists? sqlc "customCorporations")
      #t
      (query-exec sqlc "CREATE TABLE customCorporations ( corporationID INT NOT NULL, corporationTicker VARCHAR(5) NOT NULL, corporationName VARCHAR(255) NOT NULL, PRIMARY KEY ( corporationID ), UNIQUE KEY ( corporationTicker ) )")))

(define (sql-corporation-update-corporations lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO customCorporations VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE corporationID=?,corporationName=?"
		     (first x)
		     (second x)
		     (third x)
		     (first x)
		     (third x)))
	    lst))

(define (sql-corporation-create-input)
  (if (table-exists? sqlc "customCorporationInput")
      #t
      (query-exec sqlc "CREATE TABLE customCorporationInput ( corporationID INT NOT NULL, corporationTicker VARCHAR(5), corporationName VARCHAR(255), datetime DATETIME, PRIMARY KEY (corporationID) )")))

(define (sql-corporation-update-input lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO customCorporationInput VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE corporationTicker=?,corporationName=?,datetime=?"
		     (sql-corporation-id x)
		     (sql-corporation-ticker x)
		     (sql-corporation-name x)
		     (sql-corporation-datetime x)
		     (sql-corporation-ticker x)
		     (sql-corporation-name x)
		     (sql-corporation-datetime x)))
	    lst))

(define (sql-corporation-create-affiliations)
  (if (table-exists? sqlc "customCorporationAffiliations")
      #t
      (query-exec sqlc "CREATE TABLE customCorporationAffiliations ( corporationID INT NOT NULL, allianceID INT, datetime DATETIME DEFAULT '0000-00-00 00:00:00', PRIMARY KEY (corporationID) )")))

(define (sql-corporation-update-affiliations lst)
  (for-each (lambda (x) (query sqlc (string-append "INSERT INTO customCorporationAffiliations VALUES (?, ?, ?) "
						   "ON DUPLICATE KEY UPDATE allianceID=?,datetime=?")
			       (first x)
			       (second x)
			       (third x)
			       (second x)
			       (third x)))
	    lst))

(define (sql-corporation-get-affiliations)
  (query-rows sqlc (string-append "SELECT main.corporationID,c.corporationName,main.allianceID,a.allianceName,main.datetime "
				  "FROM customCorporationAffiliations as main "
				  "LEFT JOIN customCorporations AS c ON main.corporationID = c.corporationID "
				  "LEFT JOIN customAlliances AS a ON main.allianceID = a.allianceID "
				  "ORDER BY allianceName")))

;; Update moonScanMV for newly added corporations (from customCorporations)

(define (sql-corporation-create-trigger-insert)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER insert_customCorporations AFTER INSERT ON customCorporations "
		    "FOR EACH ROW BEGIN "
		    "UPDATE moonScanMV AS mv "
		    "SET "
		    "mv.corporationTicker=NEW.corporationTicker,"
		    "mv.corporationName=NEW.corporationName "
		    "WHERE mv.corporationTicker=NEW.corporationTicker; "
		    "UPDATE citadelScanMV AS mv "
		    "SET "
		    "mv.corporationTicker=NEW.corporationTicker,"
		    "mv.corporationName=NEW.corporationName "
		    "WHERE mv.corporationTicker=NEW.corporationTicker; "
		    "END;")))

;; Update moonScanMV for newly added corporation affiliations (from customCorporationAffiliations)

(define (sql-affiliation-create-trigger-insert)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER insert_customCorporationAffiliations AFTER INSERT ON customCorporationAffiliations "
		    "FOR EACH ROW BEGIN "
		    "UPDATE moonScanMV AS mv "
		    "LEFT JOIN customAlliances ON customAlliances.allianceID = NEW.allianceID "
		    "LEFT JOIN customCorporations ON customCorporations.corporationID = NEW.corporationID "
		    "SET "
		    "mv.allianceTicker=customAlliances.allianceTicker,"
		    "mv.allianceName=customAlliances.allianceName "
		    "WHERE mv.corporationTicker=customCorporations.corporationTicker; "
		    "UPDATE citadelScanMV AS mv "
		    "LEFT JOIN customAlliances ON customAlliances.allianceID = NEW.allianceID "
		    "LEFT JOIN customCorporations ON customCorporations.corporationID = NEW.corporationID "
		    "SET "
		    "mv.allianceTicker=customAlliances.allianceTicker,"
		    "mv.allianceName=customAlliances.allianceName "
		    "WHERE mv.corporationTicker=customCorporations.corporationTicker; "
		    "END;")))

(define (sql-affiliation-create-trigger-update)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER update_customCorporationAffiliations AFTER UPDATE ON customCorporationAffiliations "
		    "FOR EACH ROW BEGIN "
		    "UPDATE moonScanMV AS mv "
		    "LEFT JOIN customAlliances ON customAlliances.allianceID = NEW.allianceID "
		    "LEFT JOIN customCorporations ON customCorporations.corporationID = NEW.corporationID "
		    "SET "
		    "mv.allianceTicker=customAlliances.allianceTicker,"
		    "mv.allianceName=customAlliances.allianceName "
		    "WHERE mv.corporationTicker=customCorporations.corporationTicker; "
		    "UPDATE citadelScanMV AS mv "
		    "LEFT JOIN customAlliances ON customAlliances.allianceID = NEW.allianceID "
		    "LEFT JOIN customCorporations ON customCorporations.corporationID = NEW.corporationID "
		    "SET "
		    "mv.allianceTicker=customAlliances.allianceTicker,"
		    "mv.allianceName=customAlliances.allianceName "
		    "WHERE mv.corporationTicker=customCorporations.corporationTicker; "
		    "END;")))

(define (sql-affiliation-create-trigger-delete)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER delete_customCorporationAffiliations AFTER DELETE ON customCorporationAffiliations "
		    "FOR EACH ROW BEGIN "
		    "UPDATE moonScanMV AS mv "
		    "LEFT JOIN customCorporations ON customCorporations.corporationID = OLD.corporationID "
		    "SET "
		    "mv.allianceTicker=NULL,"
		    "mv.allianceName=NULL "
		    "WHERE mv.corporationTicker=customCorporations.corporationTicker; "
		    "UPDATE citadelScanMV AS mv "
		    "LEFT JOIN customCorporations ON customCorporations.corporationID = OLD.corporationID "
		    "SET "
		    "mv.allianceTicker=NULL,"
		    "mv.allianceName=NULL "
		    "WHERE mv.corporationTicker=customCorporations.corporationTicker; "
		    "END;")))

(define (sql-corporation-create-triggers)
  (begin
    (query-exec sqlc "DROP TRIGGER IF EXISTS insert_customCorporations")
    (sql-corporation-create-trigger-insert)
    (query-exec sqlc "DROP TRIGGER IF EXISTS insert_customCorporationAffiliations")
    (sql-affiliation-create-trigger-insert)
    (query-exec sqlc "DROP TRIGGER IF EXISTS update_customCorporationAffiliations")
    (sql-affiliation-create-trigger-update)
    (query-exec sqlc "DROP TRIGGER IF EXISTS delete_customCorporationAffiliations")
    (sql-affiliation-create-trigger-delete)))
