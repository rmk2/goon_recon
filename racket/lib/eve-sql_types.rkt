#lang racket

(require db)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define-syntax parse-type
  (syntax-rules (:members :id :group :name)
    ((_ arg) (cond
	      [(number? arg)
	       (query-row sqlc "SELECT typeID,groupID,typeName FROM invTypes WHERE typeID LIKE ?" arg)]
	      [(string? arg)
	       (query-row sqlc "SELECT typeID,groupID,typeName FROM invTypes WHERE typeName LIKE ?" arg)]))
    ((_ :members arg) (if (number? arg)
			  (query-rows sqlc "SELECT typeID,groupID,typeName FROM invTypes WHERE groupID LIKE ?" arg)
			  #f))
    ((_ :id arg) (vector-ref (parse-type arg) 0))
    ((_ :group arg) (vector-ref (parse-type arg) 1))
    ((_ :name arg) (vector-ref (parse-type arg) 2))))

(define-syntax parse-group
  (syntax-rules (:id :name)
    ((_ arg) (cond
	      [(number? arg)
	       (query-row sqlc "SELECT groupID,groupName FROM invGroups WHERE groupID LIKE ?" arg)]
	      [(string? arg)
	       (query-row sqlc "SELECT groupID,groupName FROM invGroups WHERE groupName LIKE ?" arg)]))
    ((_ :id arg) (vector-ref (parse-group arg) 0))
    ((_ :name arg) (vector-ref (parse-group arg) 1))))

(define-syntax parse-inv
  (syntax-rules (:id :group :name)
    ((_ :id v) (vector-ref v 0))
    ((_ :name v) (vector-ref (vector-take-right v 1) 0))
    ((_ :group v) (if (= (vector-length v) 3) (vector-ref v 1) #f))
    ((_ v) (parse-inv :id v))))

(define-syntax parse-solarsystem
  (syntax-rules (:region :constellation :id :name)
    ((_ arg) (cond
	      [(number? arg)
	       (query-row sqlc (string-append "SELECT regionID,constellationID,solarSystemID,solarSystemName "
					      "FROM mapSolarSystems WHERE solarSystemID  LIKE ?") arg)]
	      [(string? arg)
	       (query-row sqlc (string-append "SELECT regionID,constellationID,solarSystemID,solarSystemName "
					      "FROM mapSolarSystems WHERE solarSystemName  LIKE ?") arg)]))
    ((_ :region arg) (vector-ref (parse-solarsystem arg) 0))
    ((_ :constellation arg) (vector-ref (parse-solarsystem arg) 1))
    ((_ :id arg) (vector-ref (parse-solarsystem arg) 2))
    ((_ :name arg) (vector-ref (parse-solarsystem arg) 3))))

(define-syntax parse-constellation
  (syntax-rules (:region :id :name)
    ((_ arg) (cond
	      [(number? arg)
	       (query-row sqlc (string-append "SELECT regionID,constellationID,constellationName "
					      "FROM mapConstellations WHERE constellationID  LIKE ?") arg)]
	      [(string? arg)
	       (query-row sqlc (string-append "SELECT regionID,constellationID,constellationName "
					      "FROM mapConstellations WHERE constellationName  LIKE ?") arg)]))
    ((_ :region arg) (vector-ref (parse-constellation arg) 0))
    ((_ :id arg) (vector-ref (parse-constellation arg) 1))
    ((_ :name arg) (vector-ref (parse-constellation arg) 2))))

(define-syntax parse-region
  (syntax-rules (:id :name)
    ((_ arg) (cond
	      [(number? arg)
	       (query-row sqlc "SELECT regionID,regionName FROM mapRegions WHERE regionID LIKE ?" arg)]
	      [(string? arg)
	       (query-row sqlc "SELECT regionID,regionName FROM mapRegions WHERE regionName LIKE ?" arg)]))
    ((_ :id arg) (vector-ref (parse-region arg) 0))
    ((_ :name arg) (vector-ref (parse-region arg) 1))))

(define-syntax parse-corporation
  (syntax-rules (:id :ticker :name)
    ((_ arg)
     (cond
      [(number? arg)
       (query-row sqlc (string-append "SELECT corporationID,ticker,corporationName FROM "
				      "customCorporations WHERE corporationID LIKE ?") arg)]
      [(regexp-match #px"^[0-9]{1,}$" arg)
       (query-row sqlc (string-append "SELECT corporationID,ticker,corporationName FROM "
				      "customCorporations WHERE corporationID LIKE ?") arg)]
      [(regexp-match #px"^[A-Z0-9. -_]{1,5}$" arg)
       (query-row sqlc (string-append "SELECT corporationID,ticker,corporationName FROM "
				      "customCorporations WHERE ticker LIKE ?") arg)]
      [else
       (query-row sqlc (string-append "SELECT corporationID,ticker,corporationName FROM "
				      "customCorporations WHERE corporationNAME LIKE ?") arg)]))
    ((_ :id arg) (vector-ref (parse-corporation arg) 0))
    ((_ :ticker arg) (vector-ref (parse-corporation arg) 1))
    ((_ :name arg) (vector-ref (parse-corporation arg) 2))))

(define-syntax parse-alliance
  (syntax-rules (:id :ticker :name)
    ((_ arg)
     (cond
      [(number? arg)
       (query-row sqlc (string-append "SELECT allianceID,allianceName,allianceTicker FROM "
				      "customAlliances WHERE allianceID LIKE ?") arg)]
      [(regexp-match #px"^[0-9]{1,}$" arg)
       (query-row sqlc (string-append "SELECT allianceID,allianceName,allianceTicker FROM "
				      "customAlliances WHERE allianceID LIKE ?") arg)]
      [(regexp-match #px"^[A-Z0-9. -_]{1,5}$" arg)
       (query-row sqlc (string-append "SELECT allianceID,allianceName,allianceTicker FROM "
				      "customAlliances WHERE allianceTicker LIKE ?") arg)]
      [else
       (query-row sqlc (string-append "SELECT allianceID,allianceName,allianceTicker FROM "
				      "customAlliances WHERE allianceName LIKE ?") arg)]))
    ((_ :id arg) (vector-ref (parse-alliance arg) 0))
    ((_ :name arg) (vector-ref (parse-alliance arg) 1))
    ((_ :ticker arg) (vector-ref (parse-alliance arg) 2))))

(define-syntax parse-moon
  (syntax-rules (:name)
    ((_ arg) (query-maybe-row sqlc "SELECT itemName FROM mapDenormalize WHERE itemID = ?" arg))
    ((_ :name arg) (let ([moon-base (parse-moon arg)])
		     (if (vector? moon-base)
			 (vector-ref (parse-moon arg) 0)
			 #f)))))

(define-syntax parse-map
  (syntax-rules (:id :type :group :system :constellation :region :name)
    ((_ arg) (cond
	      [(number? arg)
	       (query-maybe-row sqlc "SELECT * FROM mapDenormalize WHERE itemID = ?" arg)]
	      [(string? arg)
	       (query-maybe-row sqlc "SELECT * FROM mapDenormalize WHERE itemName = ?" arg)]))
    ((_ :id) (vector-ref (parse-map arg) 0))
    ((_ :type) (vector-ref (parse-map arg) 1))
    ((_ :group) (vector-ref (parse-map arg) 2))
    ((_ :system) (vector-ref (parse-map arg) 3))
    ((_ :constellation) (vector-ref (parse-map arg) 4))
    ((_ :region) (vector-ref (parse-map arg) 5))
    ((_ :name) (vector-ref (parse-map arg) 6))))
