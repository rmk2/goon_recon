#lang racket

(require db)
(require srfi/19)

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")
(require "eve-sql_timerboard.rkt")
(require "eve-api_tools.rkt")

(provide (prefix-out sov: (all-defined-out)))

;; SQL functions

;; Create SQL tables

(define (sql-sov-create-campaigns-raw)
  (if (table-exists? sqlc "sovCampaignsRaw")
      #t
      (query-exec sqlc "CREATE TABLE sovCampaignsRaw ( campaignID INT NOT NULL, constellationID INT NOT NULL, constellationName VARCHAR(255), solarSystemID INT NOT NULL, solarSystemName VARCHAR(255), sovTypeID TINYINT NOT NULL, sovTypeName VARCHAR(20) NOT NULL, attackerScore DECIMAL(2,2), defenderScore DECIMAL(2,2), defenderID INT, defenderName VARCHAR(255), datetime DATETIME, PRIMARY KEY ( campaignID ) )")))

(define (sql-sov-create-campaigns-view)
  (if (table-exists? sqlc "sovCampaignsView")
      #t
      (query-exec sqlc "CREATE VIEW sovCampaignsView AS SELECT s.campaignID,r.regionID,r.regionName,s.constellationID,s.constellationName,s.solarSystemID,s.solarSystemName,s.sovTypeID,s.sovTypeName,s.attackerScore,s.defenderScore,s.defenderID,s.defenderName,datetime FROM sovCampaignsRaw AS s LEFT JOIN mapConstellations AS c ON c.constellationID = s.constellationID LEFT JOIN mapRegions AS r ON r.regionID = c.regionID")))

(define (sql-sov-prepare-campaigns-raw)
  (if (table-exists? sqlc "sovCampaignsRaw")
      (query-exec sqlc "TRUNCATE sovCampaignsRaw")
      (sql-sov-create-campaigns-raw)))

;; Update SQL tables

(define (sql-sov-update-campaigns-raw lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO sovCampaignsRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE constellationID=?,constellationName=?,solarSystemID=?,solarSystemName=?,sovTypeID=?,sovTypeName=?,attackerScore=?,defenderScore=?,defenderID=?,defenderName=?,datetime=?"
		     (sovCampaign-full-campaignid x)
		     (sovCampaign-full-constellationid x)
		     (sovCampaign-full-constellationname x)
		     (sovCampaign-full-systemid x)
		     (sovCampaign-full-systemname x)
		     (sovCampaign-full-typeid x)
		     (sovCampaign-full-typename x)
		     (sovCampaign-full-attackerscore x)
		     (sovCampaign-full-defenderscore x)
		     (sovCampaign-full-defenderid x)
		     (sovCampaign-full-defendername x)
		     (sovCampaign-full-datetime x)
		     (sovCampaign-full-constellationid x)
		     (sovCampaign-full-constellationname x)
		     (sovCampaign-full-systemid x)
		     (sovCampaign-full-systemname x)
		     (sovCampaign-full-typeid x)
		     (sovCampaign-full-typename x)
		     (sovCampaign-full-attackerscore x)
		     (sovCampaign-full-defenderscore x)
		     (sovCampaign-full-defenderid x)
		     (sovCampaign-full-defendername x)
		     (sovCampaign-full-datetime x)))
	    lst))

;; Query SQL tables

(define (sql-sov-get-campaigns-constellation constellation)
  (let ([time-now (date->string (current-date) "~1 ~3")])
    (match constellation
      [(? number? const)
       (query-rows sqlc "SELECT solarSystemID,sovTypeName,attackerScore,defenderScore,defenderName,datetime FROM sovCampaignsView WHERE constellationID = ? AND datetime <= ?" const time-now)]
      [(? string? const)
       (query-rows sqlc "SELECT solarSystemID,sovTypeName,attackerScore,defenderScore,defenderName,datetime FROM sovCampaignsView WHERE constellationName = ? AND datetime <= ?" const time-now)])))

(define (sql-sov-get-campaigns-constellation->struct constellation)
  (map (lambda (campaign) (sql-parse->struct campaign #:struct sovCampaign))
       (sql-sov-get-campaigns-constellation constellation)))

(define (get-campaigns->struct constellation)
  (sql-sov-get-campaigns-constellation->struct constellation))

(define (get-campaigns->list current-constellation)
  (map (lambda (x) (list (sovCampaign-system x)
			 (sovCampaign-type x)
			 (exact->inexact (sovCampaign-attacker-score x))
			 (exact->inexact (sovCampaign-defender-score x))
			 (sovCampaign-defender-name x)
			 (sovCampaign-datetime x)))
       (get-campaigns->struct current-constellation)))

;; Fetch sovereignty campaigns from CREST API

(define (crest-sov-get-campaigns)
  (map (lambda (hash)
	 (let ([constellation-raw (timers:json-filter :constellation hash)]
	       [system-raw (timers:json-filter :system hash)]
	       [attackers-raw (if (hash-has-key? hash 'attackers) (timers:json-filter :attackers hash) null)]
	       [defender-raw (if (hash-has-key? hash 'defender) (timers:json-filter :defender-raw hash) null)]
	       [defender-inner (if (hash-has-key? hash 'defender) (timers:json-filter :defender hash) null)])
	   (sovCampaign-full (timers:json-filter :campaign hash)
			     (timers:json-filter :id constellation-raw)
			     (timers:json-filter :name constellation-raw)
			     (timers:json-filter :id system-raw)
			     (timers:json-filter :name system-raw)
			     (timers:json-filter :type-raw hash)
			     (timers:json-filter :type hash)
			     (if (hash? attackers-raw) (timers:json-filter :score attackers-raw) 0)
			     (if (hash? defender-raw) (timers:json-filter :score defender-raw) 0)
			     (if (hash? defender-inner) (timers:json-filter :id defender-inner) 0)
			     (if (hash? defender-inner) (timers:json-filter :name defender-inner) "")
			     (timers:json-filter :time hash))))
       (timers:json-filter :items (json-api (string-append crest-root "/sovereignty/campaigns/")))))
