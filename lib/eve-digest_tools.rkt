#lang racket

(require racket/set)
(require srfi/19)
(require (for-syntax syntax/parse))

(require "eve-api_tools.rkt")
(require "eve-string_tools.rkt")
(require "eve-sql_structs.rkt")
(require "eve-sql_types.rkt")

(provide (prefix-out digest: (all-defined-out)))

;; Create and poll killboard URL

(define-syntax id/string->string
  (syntax-rules (:map)
    ((_ arg) (if (number? arg) (number->string arg) arg))
    ((_ :map lst) (map (lambda (x) (id/string->string x)) lst))))

(define (poll-url #:date [date (date->string (current-date) "~Y~m~d")]
		  #:end [end null]
		  #:groups [groups null]
		  #:types [types null]
		  #:regions [regions null]
		  #:alliances [alliances null]
		  #:corporations [corporations null]
		  #:kills [show-kills? #f]
		  #:losses [show-losses? #f]
		  #:id [killid null]
		  #:limit [limit null])
  (let ([built-url
	 (string-append
	  "https://zkillboard.com/api/no-items"
	  (cond
	   [(and show-losses? show-kills?) ""]
	   [show-losses? "/losses"]
	   [show-kills? "/kills"]
	   [else (error "Use either \"#:kills #t\" or \"#:losses #t\" when calling the pull-url function")])
	  (if (not (null? groups))
	      (string-append "/groupID/" (string-join groups ","))
	      "")
	  (if (not (null? types))
	      (string-append "/shipTypeID/" (string-join types ","))
	      "")
	  (if (not (null? regions))
	      (string-append "/regionID/" regions)
	      "")
	  (if (not (null? alliances))
	      (string-append "/allianceID/" (string-join alliances ","))
	      "")
	  (if (not (null? corporations))
	      (string-append "/corporationID/" (string-join corporations ","))
	      "")
	  (cond [(and (string? date) (= (string-length date) 8))
		 (string-append "/startTime/" (id/string->string date) "0000")]
		[(and (string? date) (= (string-length date) 12))
		 (string-append "/startTime/" (id/string->string date))]
		[else ""])
	  (if (not (null? end)) (string-append "/endTime/" end "0000") "")
	  (if (not (null? killid))
	      (string-append "/orderDirection/asc/afterKillID/" (id/string->string killid))
	      "")
	  (if (not (null? limit))
	      (string-append "/limit/" (id/string->string limit))
	      "")
	  "/")])
    (json-api built-url)))

(define (groupid->list lst)
  (append-map (lambda (i) (map (lambda (n) (vector-ref n 0)) (parse-type :members (string->number i)))) lst))

(define (concat-data #:alliances [alliances null]
		     #:corporations [corporations null]
		     #:groups [groups null]
		     #:shiptypes [shiptypes null]
		     lst)
  (define (filter-alliance lst #:filter query-alliance)
    (filter (lambda (x) (member (number->string (hash-ref x 'allianceID)) query-alliance))
	    lst))
  (define (filter-corporation lst #:filter query-corporation)
    (filter (lambda (x) (member (number->string (hash-ref x 'corporationID)) query-corporation))
	    lst))
  (define (filter-group lst #:filter query-group)
    (filter (lambda (x) (member (hash-ref x 'shipTypeID) (groupid->list query-group)))
	    lst))
  (define (filter-shiptype lst #:filter query-type)
    (filter (lambda (x) (member (hash-ref x 'shipTypeID) (map string->number query-type)))
	    lst))
  (set-intersect (if (null? alliances) lst (filter-alliance lst #:filter alliances))
		 (if (null? corporations) lst (filter-corporation lst #:filter corporations))
		 (if (null? groups) lst (filter-group lst #:filter groups))
		 (if (null? shiptypes) lst (filter-shiptype lst #:filter shiptypes))))

(define (id-tower? hash)
  (if (member (hash-ref hash 'shipTypeID) (groupid->list '("365")))
      #t
      #f))

(define-syntax (parse-helper-raw stx)
  (syntax-parse stx
		[(_ (~optional (~seq #:location location?:expr) #:defaults ([location? #f]))
		    (~optional (~seq #:moons moons?:expr) #:defaults ([moons? #f]))
		    hash:expr)
		 #'(list
		    (hash-ref hash 'shipTypeID)
		    (hash-ref hash 'characterID)
		    (hash-ref hash 'characterName)
		    (hash-ref hash 'corporationID)
		    (hash-ref hash 'corporationName)
		    (hash-ref hash 'allianceID)
		    (hash-ref hash 'allianceName))]
		[(_ (~optional (~seq #:location location?:expr) #:defaults ([location? #f]))
		    (~optional (~seq #:moons moons?:expr) #:defaults ([moons? #f]))
		    hash:expr
		    (~optional (~seq system:expr
				     moonid:expr
				     locationid:expr
				     regionid:expr
				     date:expr
				     id:expr
				     victim:expr)))
		 #'(append
		    (parse-helper-raw #:location location? #:moons moons? hash)
		    (list
		     (cond
		      [(id-tower? hash) moonid]
		      [location? locationid]
		      [else 0])
		     system
		     regionid
		     date
		     id
		     (hash-ref victim 'shipTypeID)))]))

(define-syntax (parse-helper stx)
  (syntax-parse stx
		[(_ (~optional (~seq #:location location?:expr) #:defaults ([location? #f]))
		    (~optional (~seq #:moons moons?:expr) #:defaults ([moons? #f]))
		    (~optional (~seq #:href href?:expr) #:defaults ([href? #f]))
		    hash:expr)
		 #'(list
		    (parse-type :name (hash-ref hash 'shipTypeID))
		    (hash-ref hash 'characterName)
		    (hash-ref hash 'corporationName)
		    (hash-ref hash 'allianceName))]
		[(_ (~optional (~seq #:location location?:expr) #:defaults ([location? #f]))
		    (~optional (~seq #:moons moons?:expr) #:defaults ([moons? #f]))
		    (~optional (~seq #:href href?:expr) #:defaults ([href? #f]))
		    hash:expr
		    (~optional (~seq system:expr
				     moonid:expr
				     locationid:expr
				     regionid:expr
				     date:expr
				     id:expr
				     victim:expr)))
		 #'(let ([location-base (parse-solarsystem system)])
		     (filter string?
			     (append
			      (parse-helper #:location location? #:moons moons? #:href href? hash)
			      (list
			       (cond
				[(and (id-tower? hash) moons?) (simplify-moon-display (parse-moon :name moonid))]
				[(and (id-tower? hash) location?) (parse-moon :name moonid)]
				[location? (let ([locationid-base (parse-moon :name locationid)])
					     (if (string? locationid-base)
						 locationid-base
						 (parse-universe :name location-base)))]
				[else (parse-universe :name location-base)])
			       (parse-region :name (parse-universe :region location-base))
			       date
			       (if href? (string-append "https://zkillboard.com/kill/" (number->string id) "/") #f)))))]))

(define (parse-kills #:attackers [run-attackers? #t]
		     #:raw [raw? #f]
		     #:location [location? #f]
		     #:moons [moons? #f]
		     #:href [href? #f]
		     #:alliances [alliances null]
		     #:corporations [corporations null]
		     #:groups [groups null]
		     #:shiptypes [shiptypes null]
		     lst)
  (let ([km-data lst])
    (append-map
     (lambda (km-list)
       (let* ([attackers (hash-ref km-list 'attackers)]
	      [victim (hash-ref km-list 'victim)]
	      [system (hash-ref km-list 'solarSystemID)]
	      [moonid (hash-ref km-list 'moonID)]
	      [locationid (if (hash-has-key? (hash-ref km-list 'zkb) 'locationID)
			      (hash-ref (hash-ref km-list 'zkb) 'locationID)
			      0)]
	      [regionid (parse-solarsystem :region system)]
	      [date (hash-ref km-list 'killTime)]
	      [id (hash-ref km-list 'killID)])
	 (filter-map (lambda (body)
		       (cond [(false? raw?)
			      (parse-helper #:location location? #:moons moons? #:href href?
					    body system moonid locationid regionid date id victim)]
			     [else
			      (apply sql-killmail
				     (parse-helper-raw #:location location? #:moons moons?
						       body system moonid locationid regionid date id victim))]))
		     (if run-attackers?
			 (concat-data #:alliances alliances
				      #:corporations corporations
				      #:groups groups
				      #:shiptypes shiptypes
				      attackers)
			 (concat-data #:alliances alliances
				      #:corporations corporations
				      #:groups groups
				      #:shiptypes shiptypes
				      (list victim))))))
     km-data)))
