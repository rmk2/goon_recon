#lang racket

(require db)
(require db/util/datetime)

(require "eve-sql_main.rkt")

(provide (all-from-out db/util/datetime)
	 (all-defined-out))

(define (timerboard-prepare-table)
  (if (table-exists? sqlc "customTimerboard")
      (query-exec sqlc "TRUNCATE TABLE customTimerboard")
      (query-exec sqlc "CREATE TABLE customTimerboard ( allianceName VARCHAR(255) NOT NULL, structureType VARCHAR(50) NOT NULL, solarSystemName VARCHAR(255) NOT NULL, constellationName VARCHAR(255), regionName VARCHAR(255), datetime DATETIME )")))

(define (timerboard-replace lst)
  (for-each (lambda (x)
	      (query sqlc "REPLACE INTO customTimerboard VALUES (?, ?, ?, ?, ?, ?)"
		     (first x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)))
	    lst))

(define (timerboard-create-view)
  (if (table-exists? sqlc "customTimerboardView")
      #t
      (query-exec sqlc "CREATE VIEW customTimerboardView AS SELECT t.regionName,t.constellationName,t.solarSystemName,t.structureType,a.allianceTicker,t.allianceName,t.datetime FROM customTimerboard AS t LEFT JOIN customAlliances AS a ON a.allianceName = t.allianceName")))
