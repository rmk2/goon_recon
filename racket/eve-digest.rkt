#! /usr/bin/env racket
#lang racket

(require eve)
(require "eve-sql.rkt")
(require racket/set)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))
(require srfi/19)

(define cl-date (make-parameter (date->string (current-date) "~Y~m~d")))
(define cl-end (make-parameter null))
(define cl-href (make-parameter #f))
(define cl-csv (make-parameter #f))
(define cl-active (make-parameter #f))
(define cl-losses (make-parameter #f))
(define cl-kills (make-parameter #t))
(define cl-groups (make-parameter null))
(define cl-regions (make-parameter null))
(define cl-alliances (make-parameter null))
(define cl-shiptypes (make-parameter null))

(define cl-html (make-parameter #f))

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

;; Functions to let us find alliances either by ID, ticker or name

(define alliances (rowset->hash
		   (string->xexpr
		    (xml-api "https://api.eveonline.com/eve/AllianceList.xml.aspx?version=1"))))

(define-syntax parse-alliance
  (syntax-rules (:id :ticker :name)
    ((_ query) (case (filter-id query)
		 [("Number") (findf (lambda (lst) (equal? query (hash-ref lst 'allianceID))) alliances)]
		 [("Ticker") (findf (lambda (lst) (equal? query (hash-ref lst 'shortName))) alliances)]
		 [("Name") (findf (lambda (lst) (regexp-match query (hash-ref lst 'name))) alliances)]))
    ((_ :id query) (hash-ref (parse-alliance query) 'allianceID))
    ((_ :ticker query) (hash-ref (parse-alliance query) 'shortName))
    ((_ :name query) (hash-ref (parse-alliance query) 'name))))

;; Command-line argument handling

(define parse-args
  (command-line
   #:multi
   [("-r" "--region") str "Select regions to use in the query, default: false" (cl-regions (cons (region->id str) (cl-regions)))]
   [("-A" "--alliance") str "Filter by alliance ID, default: false" (cl-alliances (cons (parse-alliance :id str) (cl-alliances)))]
   [("-g" "--group") str "Select a groupid, default: false" (cl-groups (cons (group->id str) (cl-groups)))]
   [("-t" "--type") str "Select a typeid, default: false" (cl-shiptypes (cons (type->id str) (cl-shiptypes)))]
   #:once-each
   [("-d" "--date") str "Select start date, format: YYYYMMDD" (cl-date str)]
   [("-e" "--end-date") str "Select end date, format: YYYYMMDD" (cl-end str)]
   [("-l" "--link" "--href") "Show links to killmails, default: false" (cl-href #t)]
   [("-P" "--pilot" "--active") "Show a list of (unique) active pilots" (cl-active #t)]
   [("-H" "--html") "Output parsed data as html, default: false" (begin (cl-csv #f) (cl-html #t))]
   [("-c" "--csv" "-p" "--print") "Print output as csv, default: false" (begin (cl-html #f) (cl-csv #t))]
   #:once-any
   [("-a" "--all") "Show kills & losses by <groupid>, default: false" (begin (cl-kills #t) (cl-losses #t))]
   [("-k" "--kills") "Show kills by <groupid>, default: true" (begin (cl-kills #t) (cl-losses #f))]
   [("-L" "--losses") "Show losses by <groupid>, default: false" (begin (cl-losses #t) (cl-kills #f))]))

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
		  #:kills [show-kills? (cl-kills)]
		  #:losses [show-losses? (cl-losses)])
  (let ([built-url
	 (string-append
	  "https://zkillboard.com/api/no-items/"
	  "startTime/" (id/string->string date) "0000"
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
	  (if (not (null? (cl-end))) (string-append "/endTime/" (cl-end) "0000") "")
	  (if (not (null? alliances))
	      (string-append "/allianceID/" (string-join alliances ","))
	      ""))])
    (json-api built-url)))

(define (groupid->list lst)
  (append-map (lambda (i) (map (lambda (n) (vector-ref n 0)) (parse-type :members (string->number i)))) lst))

(define (map-string-number lst) (map (lambda (i) (string->number i)) lst))

(define-syntax concat-data
  (syntax-rules (:alliance :group :shiptype :check)
    ((_ :alliance lst) (filter-map (lambda (x)
				     (cond
				      [(null? (cl-alliances)) x]
				      [(member (number->string (hash-ref x 'allianceID)) (cl-alliances)) x]
				      [else #f]))
				   lst))
    ((_ :group lst) (filter-map (lambda (x)
				  (cond
				   [(null? (cl-groups)) x]
				   [(member (hash-ref x 'shipTypeID) (groupid->list (cl-groups))) x]
				   [else #f]))
				lst))
    ((_ :shiptype lst) (filter-map (lambda (x)
				     (cond
				      [(null? (cl-shiptypes)) x]
				      [(member (hash-ref x 'shipTypeID) (map-string-number (cl-shiptypes))) x]
				      [else #f]))
				   lst))
    ((_ :check lst) (set-intersect (concat-data :alliance lst)
				   (concat-data :group lst)
				   (concat-data :shiptype lst)))))

(define-syntax parse-helper
  (syntax-rules ()
    ((_ hash)
     (list
      (parse-type :name (hash-ref hash 'shipTypeID))
      (hash-ref hash 'characterName)
      (hash-ref hash 'corporationName)
      (hash-ref hash 'allianceName)))
    ((_ hash location date id)
     (filter string?
	     (append
	      (parse-helper hash)
	      (list
	       (parse-solarsystem :name location)
	       (parse-region :name (parse-solarsystem :region location))
	       date
	       (if (cl-href) (string-append "https://zkillboard.com/kill/" (number->string id) "/") #f)))))))

(define (parse-kills lst #:attackers [run-attackers? #t])
  (let ([km-data lst])
    (append-map (lambda (km-list)
		  (let ([victim (hash-ref km-list 'victim)]
			[date (hash-ref km-list 'killTime)]
			[location (hash-ref km-list 'solarSystemID)]
			[attackers (hash-ref km-list 'attackers)]
			[id (hash-ref km-list 'killID)])
		    (filter-map (lambda (a) (parse-helper a location date id))
				(if run-attackers?
				    (concat-data :check attackers)
				    (concat-data :check (list victim))))))
		km-data)))

;; Workaround for zkill's limitiation of one region per query, which cannot be
;; concatened via commas in one request, either. Instead, we have to do
;; sequential polls, which we can however append in order to parse them all
;; together. We also re-sort the combined region list by date to keep things
;; legible.

(define (run-regions lst)
  (sort
   (append-map (lambda (current-region) (pull-url #:regions current-region)) lst)
   string<?
   #:key (lambda (h) (hash-ref h 'killTime))
   #:cache-keys? #t))
  
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
     (format "Results filtered for: Alliance (~a), Shipgroup (~a), Shiptype (~a), Region (~a), since (~a), last updated (~a)"
	     (string-join
	      (map (lambda (a) (parse-alliance :name a)) (cl-alliances)) "|")
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
      (head
       (title "EVE Killboard Digest")
       (literal (style/inline 'type: "text/css" ".data { margin: 1em 3%; }"))
       (literal (style/inline 'type: "text/css" "table { border-collapse: collapse;  border: 1px solid black; width: 100%; }"))
       (literal (style/inline 'type: "text/css" "thead { border-bottom: 1px solid black; }"))
       (literal (style/inline 'type: "text/css" "td { padding: 0.3em; border-right: 1px solid black; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 15.5em; }"))
       (literal (style/inline 'type: "text/css" "tr:nth-child(2n+1) > td { background-color: #efefef; }"))
       (literal (style/inline 'type: "text/css" "th.header { background: url(\"data:image/gif;base64, R0lGODlhFQAJAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAkAAAIXjI+AywnaYnhUMoqt3gZXPmVg94yJVQAAOw==\") no-repeat 99% ; margin-left: -1px; background-position: center left; padding: .2em 1.33em; text-align: left; } th.headerSortUp { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjB+gC+jP2ptn0WskLQA7\") no-repeat 99% ; } th.headerSortDown { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjI8Bya2wnINUMopZAQA7\") no-repeat 99% ; }"))
       (script 'type: "text/javascript" 'src: "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")
       (script 'type: "text/javascript" 'src: "./jquery.tablesorter.min.js")
       ;; (script (literal "$(document).ready(function() { $(\"#killmails\").tablesorter( { sortList: [[6,0]] } ); });")))
       (script (literal "$(document).ready(function() { $(\"*\").tablesorter( { sortList: [[6,0]] } ); });")))
      (body
       (div 'id: "content"
	    (h1 "EVE Killboard Digest")
	    (create-html-filter)
	    (p 'style: "padding-left:.2em" "Hint: hold down SHIFT to select multiple columns for sorting")
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
			(parse-kills (run-regions (cl-regions)) #:attackers #t)
			null))

(define cache-losses (if (cl-losses)
			 (parse-kills (run-regions (cl-regions))  #:attackers #f)
			 null))

(define-values (active attackers victims)
  (values
   (if (cl-active)
       (cons "Active Pilots (last appearance)" (unique-car (append cache-kills cache-losses) second))
       #f)
   (if (not (empty? cache-kills))
       (cons "Attackers" cache-kills)
       #f)
   (if (not (empty? cache-losses))
       (cons "Victims" cache-losses)
       #f)))

;; Exec

(let ([data (filter list? (list active attackers victims))])
  (cond
   [(cl-html) (output-html data)]
   [(cl-csv) (print-csv (output-csv data))]
   [else data]))
