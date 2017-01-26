#lang racket

(require db)
(require db/util/datetime)
(require srfi/19)

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")
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

;; Replace customTimerboardView with a view based on sovCampaignsView

(define (sql-sov-create-timerboard-view)
  (if (table-exists? sqlc "sovTimerboardView")
      #t
      (query-exec sqlc "CREATE VIEW sovTimerboardView AS SELECT s.regionName,s.constellationName,s.solarSystemName,s.sovTypeName AS structureType,IF(s.sovTypeID=4,'',a.allianceTicker) AS allianceTicker,IF(s.sovTypeID=4,'*free-for-all',a.allianceName) AS allianceName,s.datetime FROM sovCampaignsView AS s LEFT JOIN customAlliances AS a ON s.defenderID = a.allianceID")))

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

;; JSON parsing

(define-syntax json-filter
  (syntax-rules (:name :score :id :id_str :campaign :defender-raw :defender
		       :defender-name :attackers :system :system-name
		       :constellation :time :score :type-raw :type :constellation-id
		       :href :region :items)
    ((_ :name f) (hash-ref f 'name))
    ((_ :score f) (hash-ref f 'score))
    ((_ :id f) (hash-ref f 'id))
    ((_ :id_str f) (hash-ref f 'id_str))
    ((_ :campaign hash) (hash-ref hash 'campaignID))
    ((_ :defender-raw hash) (hash-ref hash 'defender))
    ((_ :defender hash) (hash-ref (json-filter :defender-raw hash) 'defender))
    ((_ :defender-name hash) (json-filter :name (json-filter :defender hash)))
    ((_ :attackers hash) (hash-ref hash 'attackers))
    ((_ :system hash) (hash-ref hash 'sourceSolarsystem))
    ((_ :system-name hash) (json-filter :name (json-filter :system hash)))
    ((_ :constellation hash) (hash-ref hash 'constellation))
    ((_ :constellation-id hash) (hash-ref (json-filter :constellation hash) 'id_str))
    ((_ :time hash) (hash-ref hash 'startTime))
    ((_ :type-raw hash) (hash-ref hash 'eventType))
    ((_ :type hash) (case (hash-ref hash 'eventType)
		      [(1) "TCU"]
		      [(2) "IHUB"]
		      [(3) "Station"]
		      [(4) "Freeport"]))
    ((_ :href hash) (hash-ref hash 'href))
    ((_ :region hash) (hash-ref hash 'region))
    ((_ :items hash) (hash-ref hash 'items))))

;; Fetch sovereignty campaigns from CREST API

(define (crest-sov-get-campaigns)
  (map (lambda (hash)
	 (let ([constellation-raw (json-filter :constellation hash)]
	       [system-raw (json-filter :system hash)]
	       [attackers-raw (if (hash-has-key? hash 'attackers) (json-filter :attackers hash) null)]
	       [defender-raw (if (hash-has-key? hash 'defender) (json-filter :defender-raw hash) null)]
	       [defender-inner (if (hash-has-key? hash 'defender) (json-filter :defender hash) null)])
	   (sovCampaign-full (json-filter :campaign hash)
			     (json-filter :id constellation-raw)
			     (json-filter :name constellation-raw)
			     (json-filter :id system-raw)
			     (json-filter :name system-raw)
			     (json-filter :type-raw hash)
			     (json-filter :type hash)
			     (if (hash? attackers-raw) (json-filter :score attackers-raw) 0)
			     (if (hash? defender-raw) (json-filter :score defender-raw) 0)
			     (if (hash? defender-inner) (json-filter :id defender-inner) 0)
			     (if (hash? defender-inner) (json-filter :name defender-inner) "")
			     (json-filter :time hash))))
       (json-filter :items (json-api (string-append crest-root "/sovereignty/campaigns/")))))
