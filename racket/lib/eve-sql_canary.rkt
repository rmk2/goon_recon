#lang racket

(require db)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define (sql-canary-create-corporations)
  (if (table-exists? sqlc "canaryCorporations")
      #t
      (query-exec sqlc "CREATE VIEW canaryCorporations AS SELECT corporationID,corporationName,allianceID,allianceName,AVG(killcount) killAvg,STDDEV_SAMP(killcount) killStd FROM intelSuperWatchlist GROUP BY corporationID ORDER BY allianceName")))

(define (sql-canary-create-alliances)
  (if (table-exists? sqlc "canaryAlliances")
      #t
      (query-exec sqlc "CREATE VIEW canaryAlliances AS SELECT allianceID,allianceName,AVG(killcount) killAvg,STDDEV_SAMP(killcount) killStd FROM intelSuperWatchlist GROUP BY allianceID ORDER BY allianceName")))

(define (sql-canary-get-watchlist #:show-losses [show-losses? #f] #:corporations [corporations? #f])
  (query-rows sqlc (string-append "SELECT shipTypeName,characterName,canary.corporationName,canary.allianceName,"
				  "solarSystemname,regionName,date(datetime) "
				  "FROM intelSuperWatchlist AS canary "
				  (if corporations?
				      "JOIN canaryCorporations AS stat ON canary.corporationID = stat.corporationID "
				      "JOIN canaryAlliances AS stat ON canary.allianceID = stat.allianceID ")
				  "WHERE killCount >= (killStd*2) "
				  "AND DATEDIFF(CURDATE(),datetime) <= 90 "
				  (if show-losses? "" "AND eventType != 'Loss' ")
				  "ORDER BY canary.allianceName,datetime,regionName,killCount DESC;")))
