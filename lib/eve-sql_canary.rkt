#lang racket

(require db)
(require db/util/datetime)
(require srfi/19)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define (sql-canary-create-corporations)
  (if (table-exists? sqlc "canaryCorporations")
      #t
      (query-exec sqlc "CREATE VIEW canaryCorporations AS SELECT corporationID,corporationName,allianceID,allianceName,AVG(killcount) killAvg,STDDEV_SAMP(killcount) killStd FROM intelSuperLatestMV GROUP BY corporationID ORDER BY allianceName")))

(define (sql-canary-create-alliances)
  (if (table-exists? sqlc "canaryAlliances")
      #t
      (query-exec sqlc "CREATE VIEW canaryAlliances AS SELECT allianceID,allianceName,AVG(killcount) killAvg,STDDEV_SAMP(killcount) killStd FROM intelSuperLatestMV GROUP BY allianceID ORDER BY allianceName")))

(define (sql-canary-get-watchlist #:show-losses [show-losses? #f] #:corporations [corporations? #f])
  (let ([today (srfi-date->sql-date (current-date))])
    (query-rows sqlc (string-append "SELECT shipTypeName,characterName,canary.corporationName,canary.allianceName,"
				    "solarSystemName,regionName,date(datetime),DATE_FORMAT(activityAvg,'%H:%m:%s'),activityStd "
				    "FROM intelSuperWatchlist AS canary "
				    (if corporations?
					"JOIN canaryCorporations AS stat ON canary.corporationID = stat.corporationID "
					"JOIN canaryAlliances AS stat ON canary.allianceID = stat.allianceID ")
				    "WHERE killCount >= (killAvg + killStd) "
				    "AND datetime >= date_sub(?, INTERVAL 60 DAY) "
				    (if show-losses? "" "AND eventType != 'Loss' ")
				    "ORDER BY canary.allianceName,datetime,regionName,killCount DESC;")
		today)))

(define (sql-canary-create-watchlist-alliances)
  (if (table-exists? sqlc "intelCanaryAlliances")
      #t
      (query-exec sqlc (string-append
			"CREATE VIEW intelCanaryAlliances AS "
			"SELECT "
			"shipTypeName,characterName,corporationTicker,canary.corporationName,allianceTicker,"
			"canary.allianceName,solarSystemName,constellationName,regionName,date(datetime) AS date,"
			"DATE_FORMAT(activityAvg,'%H:%m:%s') AS activityAvg,activityStd "
			"FROM intelSuperWatchlist AS canary "
			"LEFT JOIN canaryAlliances AS stat ON canary.allianceID = stat.allianceID "
			;; "WHERE datetime >= date_sub(curdate(), INTERVAL 60 DAY) "
			"WHERE killCount >= (killAvg + killStd) "
			"AND eventType != 'Loss' "
			"ORDER BY canary.allianceName,datetime,regionName,killCount DESC;"))))

(define (sql-canary-create-watchlist-corporations)
  (if (table-exists? sqlc "intelCanaryCorporations")
      #t
      (query-exec sqlc (string-append
			"CREATE VIEW intelCanaryCorporations AS "
			"SELECT "
			"shipTypeName,characterName,corporationTicker,canary.corporationName,allianceTicker,"
			"canary.allianceName,solarSystemName,constellationName,regionName,date(datetime) AS date,"
			"DATE_FORMAT(activityAvg,'%H:%m:%s') AS activityAvg,activityStd "
			"FROM intelSuperWatchlist AS canary "
			"LEFT JOIN canaryCorporations AS stat ON canary.corporationID = stat.corporationID "
			;; "WHERE datetime >= date_sub(curdate(), INTERVAL 60 DAY) "
			"WHERE killCount >= (killAvg + killStd) "
			"AND eventType != 'Loss' "
			"ORDER BY canary.allianceName,canary.corporationName,datetime,regionName,killCount DESC;"))))
