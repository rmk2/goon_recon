#lang racket

(require db)
(require db/util/datetime)

(require "eve-sql_main.rkt")
(require "eve-sql_types.rkt")
(require "eve-api_tools.rkt")

(provide (all-from-out db/util/datetime)
	 (prefix-out timers: (all-defined-out)))

;; SQL tables

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

;; API handling

(define api (json-api "https://crest-tq.eveonline.com/sovereignty/campaigns/"))

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

(define-syntax freeport?
  (syntax-rules ()
    ((_ hash) (if (= (json-filter :type-raw hash) 4) #t #f))))

;; API Parsing (JSON->list)

(define (query-sovereignty-timers)
  (let ([query-data (hash-ref api 'items)])
    (map (lambda (x) (list
		      (if (freeport? x) "*free-for-all" (json-filter :name (json-filter :defender x)))
		      (json-filter :type x)
		      (json-filter :name (json-filter :system x))
		      (json-filter :name (json-filter :constellation x))
		      (parse-region :name (parse-constellation :region (string->number (json-filter :constellation-id x))))
		      (json-filter :time x)))
	 query-data)))
