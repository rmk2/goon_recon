#! /usr/bin/env racket
#lang racket

(require eve)
(require racket/set)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))
(require racket/future)

(define cl-date (make-parameter (date->string (current-date) "~Y~m~d")))
(define cl-end (make-parameter null))
(define cl-id (make-parameter null))
(define cl-href (make-parameter #f))
(define cl-quiet (make-parameter #f))
(define cl-active (make-parameter #f))
(define cl-losses (make-parameter #f))
(define cl-kills (make-parameter #t))
(define cl-groups (make-parameter null))
(define cl-regions (make-parameter null))
(define cl-alliances (make-parameter null))
(define cl-shiptypes (make-parameter null))
(define cl-corporations (make-parameter null))
(define cl-moons (make-parameter #f))
(define cl-location (make-parameter #f))

(define cl-html (make-parameter #f))
(define cl-csv (make-parameter #f))
(define cl-raw (make-parameter #f))
(define cl-sql (make-parameter #f))

;; Wrapper to use "futures" whenever more than one core is available (thanks EDIS...)

(define-syntax future-wrapper
  (syntax-rules (:future :touch)
    ((_ :future func) (if (> (processor-count) 1)
			  (future (lambda () func))
			  func))
    ((_ :touch func) (if (future? func)
			 (touch func)
			 func))
    ((_ func) (if (future? func)
		  (future-wrapper :touch func)
		  (future-wrapper :future func)))))

;; Find out whether input (which is always a string) is actually a string, or a number

(define-syntax filter-id
  (syntax-rules ()
    ((_ str) (cond
	      [(regexp-match #px"^[0-9]{1,}$" str) "Number"]
	      [(regexp-match #px"^[A-Z0-9. -_]{1,5}$" str) "Ticker"]
	      [else "Name"]))))

;; Functions to parse names into type/group/region IDs

(define (type->id str)
  (case (filter-id str)
    [("Name") (number->string (parse-type :id str))]
    [else str]))

(define (group->id str)
  (case (filter-id str)
    [("Name") (number->string (parse-group :id str))]
    [else str]))

(define (region->id str)
  (case (filter-id str)
    [("Name") (number->string (parse-region :id str))]
    [else str]))

;; Command-line argument handling

(define parse-args
  (command-line
   #:multi
   [("-r" "--region") str "Select regions to use in the query, default: false" (cl-regions (cons (region->id str) (cl-regions)))]
   [("-A" "--alliance") str "Filter by alliance ID, default: false"
    (cl-alliances (cons (id/string->string (parse-alliance :id str)) (cl-alliances)))]
   [("-g" "--group") str "Select a groupid, default: false" (cl-groups (cons (group->id str) (cl-groups)))]
   [("-t" "--type") str "Select a typeid, default: false" (cl-shiptypes (cons (type->id str) (cl-shiptypes)))]
   [("-C" "--corporation" "--corp") str "Filter by corporation ID, default: false"
    (cl-corporations (cons (id/string->string (parse-corporation :id str)) (cl-corporations)))]
   #:once-each
   [("-d" "--date") str "Select start date, format: YYYYMMDD" (cl-date str)]
   [("-e" "--end-date") str "Select end date, format: YYYYMMDD" (cl-end str)]
   [("-l" "--link" "--href") "Show links to killmails, default: false" (cl-href #t)]
   [("-P" "--pilot" "--active") "Show a list of (unique) active pilots" (cl-active #t)]
   [("-H" "--html") "Output parsed data as html, default: false" (begin (cl-csv #f) (cl-html #t))]
   [("-c" "--csv" "-p" "--print") "Print output as csv, default: false" (begin (cl-html #f) (cl-csv #t))]
   [("-q" "--quiet") "Print Active Pilots only, suppress other output, default: false" (begin (cl-active #t) (cl-quiet #t))]
   [("-R" "--raw") "Do not parse TypeIDs before outputting them, default: false" (cl-raw #t)]
   [("-S" "--sql") "Output data to SQL, requires --raw, default: false" (begin (cl-raw #t) (cl-sql #t))]
   [("-m" "--moon" "--moons") "Print moon instead of system for towers, default: false" (cl-moons #t)]
   [("-M" "--location" "--locations") "Print location information, default: false" (cl-location #t)]
   [("-i" "--id" "--kill-id") str "Use a killID instead of a date as fetch point" (begin (cl-date null) (cl-id str))]
   #:once-any
   [("-a" "--all") "Show kills & losses by <groupid>, default: false" (begin (cl-kills #t) (cl-losses #t))]
   [("-k" "--kills") "Show kills by <groupid>, default: true" (begin (cl-kills #t) (cl-losses #f))]
   [("-L" "--losses") "Show losses by <groupid>, default: false" (begin (cl-losses #t) (cl-kills #f))]
   [("-X" "--cron" "--intel") "Shortcut for collecting super killmails via cron, setting all appropriate options"
    (begin (cl-raw #t) (cl-sql #t) (cl-losses #t) (cl-kills #t) (cl-date null) (cl-groups '("30" "659"))
	   (cl-id (sql-super-latest-killid)))]))

;; Parse zkillboard data

(define-syntax id/string->string
  (syntax-rules (:map)
    ((_ arg) (if (number? arg) (number->string arg) arg))
    ((_ :map lst) (map (lambda (x) (id/string->string x)) lst))))

(define (pull-url #:date [date (cl-date)]
		  #:groups [groups (cl-groups)]
		  #:types [types (cl-shiptypes)]
		  #:regions [regions (cl-regions)]
		  #:alliances [alliances (cl-alliances)]
		  #:corporations [corporations (cl-corporations)]
		  #:kills [show-kills? (cl-kills)]
		  #:losses [show-losses? (cl-losses)]
		  #:id [killid (cl-id)])
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
	  (if (not (null? date)) (string-append "/startTime/" (id/string->string date) "0000") "")
	  (if (not (null? (cl-end))) (string-append "/endTime/" (cl-end) "0000") "")
	  (if (not (null? killid))
	      (string-append "/orderDirection/asc/afterKillID/" (id/string->string killid))
	      "")
	  "/")])
    (json-api built-url)))

(define (groupid->list lst)
  (append-map (lambda (i) (map (lambda (n) (vector-ref n 0)) (parse-type :members (string->number i)))) lst))

(define-syntax concat-data
  (syntax-rules (:alliance :corporation :group :shiptype :check)
    ((_ :alliance lst) (filter-map (lambda (x)
				     (cond
				      [(member (number->string (hash-ref x 'allianceID)) (cl-alliances)) x]
				      [else #f]))
				   lst))
    ((_ :corporation lst) (filter-map (lambda (x)
					(cond
					 [(member (number->string (hash-ref x 'corporationID)) (cl-corporations)) x]
					 [else #f]))
				      lst))
    ((_ :group lst) (filter-map (lambda (x)
				  (cond
				   [(member (hash-ref x 'shipTypeID) (groupid->list (cl-groups))) x]
				   [else #f]))
				lst))
    ((_ :shiptype lst) (filter-map (lambda (x)
				     (cond
				      [(member (hash-ref x 'shipTypeID) (map string->number (cl-shiptypes))) x]
				      [else #f]))
				   lst))
    ((_ :check lst) (set-intersect (if (null? (cl-alliances)) lst (concat-data :alliance lst))
				   (if (null? (cl-corporations)) lst (concat-data :corporation lst))
				   (if (null? (cl-groups)) lst (concat-data :group lst))
				   (if (null? (cl-shiptypes)) lst (concat-data :shiptype lst))))))

(define (tower? hash)
  (if (member (hash-ref hash 'shipTypeID) (groupid->list '("365")))
      #t
      #f))

(define-syntax parse-helper
  (syntax-rules ()
    ((_ hash)
     (list
      (parse-type :name (hash-ref hash 'shipTypeID))
      (hash-ref hash 'characterName)
      (hash-ref hash 'corporationName)
      (hash-ref hash 'allianceName)))
    ((_ hash location moonid locationid date id)
     (let ([location-base (parse-solarsystem location)])
       (filter string?
	       (append
		(parse-helper hash)
		(list
		 (cond
		  [(and (tower? hash) (cl-moons)) (simplify-moon-display (parse-moon :name moonid))]
		  [(and (tower? hash) (cl-location)) (parse-moon :name moonid)]
		  [(cl-location) (let ([locationid-base (parse-moon :name locationid)])
				   (if (string? locationid-base)
				       locationid-base
				       (parse-universe :name location-base)))]
		  [else (parse-universe :name location-base)])
		 (parse-region :name (parse-universe :region location-base))
		 date
		 (if (cl-href) (string-append "https://zkillboard.com/kill/" (number->string id) "/") #f))))))))

(define-syntax parse-helper-raw
  (syntax-rules ()
    ((_ hash)
     (list
      (hash-ref hash 'shipTypeID)
      (hash-ref hash 'characterID)
      (hash-ref hash 'characterName)
      (hash-ref hash 'corporationID)
      (hash-ref hash 'corporationName)
      (hash-ref hash 'allianceID)
      (hash-ref hash 'allianceName)))
    ((_ hash location moonid locationid date id victim)
     (append
      (parse-helper-raw hash)
      (list
       (cond
	[(tower? hash) moonid]
	[(cl-location) locationid]
	[else 0])
       location
       (parse-region :id (parse-solarsystem :region location))
       date
       id
       (hash-ref victim 'shipTypeID))))))

(define (parse-kills lst #:attackers [run-attackers? #t])
  (let ([km-data lst])
    (append-map (lambda (km-list)
		  (let ([victim (hash-ref km-list 'victim)]
			[date (hash-ref km-list 'killTime)]
			[location (hash-ref km-list 'solarSystemID)]
			[moonid (hash-ref km-list 'moonID)]
			[locationid (hash-ref (hash-ref km-list 'zkb) 'locationID)]
			[attackers (hash-ref km-list 'attackers)]
			[id (hash-ref km-list 'killID)])
		    (filter-map (lambda (a) (if (cl-raw)
						(parse-helper-raw a location moonid locationid date id victim)
						(parse-helper a location moonid locationid date id)))
				(if run-attackers?
				    (concat-data :check attackers)
				    (concat-data :check (list victim))))))
		km-data)))

;; Workaround for zkill's limitiation of one region per query, which cannot be
;; concatened via commas in one request, either. Instead, we have to do
;; sequential polls, which we can however append in order to parse them all
;; together. We also re-sort the combined region list by date to keep things
;; legible.

(define (run-regions lst #:kills [show-kills? #f] #:losses [show-losses? #f])
  (if (empty? lst)
      (pull-url)
      (sort (append-map (lambda (current-region) (pull-url #:regions current-region
							   #:kills show-kills?
							   #:losses show-losses?))
			lst)
	    string<?
	    #:key (lambda (h) (hash-ref h 'killTime))
	    #:cache-keys? #t)))

;; HTML Output

(define (create-html-table lst)
  (if (empty? (cdr lst))
      #f
      (let ([id (string-downcase (car (regexp-match #px"^\\w+" (car lst))))])
	(div 'class: "data"
	     (h2 (car lst))
	     (table 'id: id 'class: "tablesorter"
		    (thead (tr (th "Shiptype")
			       (th "Name")
			       (th "Corporation")
			       (th "Alliance")
			       (th "System")
			       (th "Region")
			       (th "Date")
			       (if (cl-href) (th "Link") null)))
		    (tbody
		     (map (lambda (data) (tr (map (lambda (str) (td (if (regexp-match #px"^http" str)
									(a 'href: str 'target: "_blank" "-> link")
									str))) data)))
			  (cdr lst))))))))

(define (create-html-filter)
  (p 'id: "filter" 'style: "padding-left:.2em"
     (format "Results filtered for: Alliance (~a), Corporation (~a), Shipgroup (~a), Shiptype (~a), Region (~a), since (~a), last updated (~a)"
	     (string-join
	      (map (lambda (a) (parse-alliance :name a)) (cl-alliances)) "|")
	     (string-join
	      (map (lambda (a) (parse-corporation :name a)) (cl-corporations)) "|")
	     (string-join
	      (map (lambda (g) (parse-group :name (string->number (group->id g)))) (cl-groups)) "|")
	     (string-join
	      (map (lambda (t) (parse-type :name (string->number (type->id t)))) (cl-shiptypes)) "|")
	     (string-join
	      (map (lambda (r) (parse-region :name (string->number (region->id r)))) (cl-regions)) "|")
	     (date->string (string->date (cl-date) "~Y~m~d") "~5")
	     (date->string (current-date) "~5"))))

(define (create-html-navigation lst)
  (div 'id: "navigation" 'class: "navbar"
       (map (lambda (link) (a 'class: "nav-ele" 'href: (string-downcase (string-append link ".html")) link))lst)))

(define (output-html lst)
  (begin
    (output-xml (doctype 'html))
    (output-xml
     (html
      (output:create-html-head #:title "EVE Killboard Digest" #:sort-column 6)
      (body
       (div 'id: "content"
	    (h1 "EVE Killboard Digest")
	    (create-html-filter)
	    (output:create-html-hint :tablesorter)
	    (filter-map (lambda (t) (create-html-table t)) lst)))))))

;; CSV Output

(define (output-csv lst)
  (append-map (lambda (l) (list* "-------------"
				 (string-append "> " (car l))
				 "-------------"
				 (input-map-join (cdr l))))
	      lst))

(define (print-csv lst)
  (for-each (lambda (x) (displayln x))
	    lst))

;; Generic Output

(define cache-kills (if (cl-kills)
			(future-wrapper :future (parse-kills (run-regions (cl-regions) #:kills #t) #:attackers #t))
			null))

(define cache-losses (if (cl-losses)
			 (future-wrapper :future (parse-kills (run-regions (cl-regions) #:losses #t)  #:attackers #f))
			 null))

(define-values (active attackers victims)
  (values
   (if (cl-active)
       (cons "Active Pilots (last appearance)"
	     (unique-car (append
			  (future-wrapper :touch cache-kills)
			  (future-wrapper :touch cache-losses))
			 second))
       #f)
   (if (and (not (cl-quiet))
	    (not (empty? (future-wrapper :touch cache-kills))))
       (cons "Attackers" (future-wrapper :touch cache-kills))
       #f)
   (if (and (not (cl-quiet))
	    (not (empty? (future-wrapper :touch cache-losses))))
       (cons "Victims" (future-wrapper :touch cache-losses))
       #f)))

;; Exec

(let ([data (filter list? (list active attackers victims))])
  (cond
   [(cl-html) (output-html data)]
   [(cl-csv) (print-csv (output-csv data))]
   [(cl-sql) (super-replace-killmails
	      (append
	       (map (lambda (x) (flatten (append x "Kill"))) (future-wrapper :touch cache-kills))
	       (map (lambda (x) (flatten (append x "Loss"))) (future-wrapper :touch cache-losses))))]
   [else data]))
