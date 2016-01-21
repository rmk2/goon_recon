#! /usr/bin/env racket
#lang racket

(require eve)
(require "eve-sql.rkt")
(require racket/set)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

(define cl-date (make-parameter (date->string (current-date) "~Y~m~d")))
(define cl-end (make-parameter null))
(define cl-href (make-parameter #f))
(define cl-losses (make-parameter #f))
(define cl-kills (make-parameter #t))
(define cl-groups (make-parameter null))
(define cl-regions (make-parameter null))
(define cl-alliances (make-parameter null))

;; Found out whether input (which is always a string) is actually a string, or a number

(define-syntax filter-id
  (syntax-rules ()
    ((_ str) (cond
	      [(regexp-match #px"^[0-9]{1,8}$" str) "Number"]
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

;; Command-line argument handling

(define-syntax parse-alliance
  (syntax-rules (:id :ticker :name)
    ((_ query) (case (filter-id query)
		 [("Number") (findf (lambda (lst) (equal? query (hash-ref lst 'allianceID))) alliances)]
		 [("Ticker") (findf (lambda (lst) (equal? query (hash-ref lst 'shortName))) alliances)]
		 [("Name") (findf (lambda (lst) (regexp-match query (hash-ref lst 'name))) alliances)]))
    ((_ :id query) (hash-ref (parse-alliance query) 'allianceID))
    ((_ :ticker query) (hash-ref (parse-alliance query) 'shortName))
    ((_ :name query) (hash-ref (parse-alliance query) 'name))))

(define parse-args
  (command-line
   #:multi
   [("-r" "--region") str "Select regions to use in the query, default: false" (cl-regions (cons (region->id str) (cl-regions)))]
   [("-A" "--alliance") str "Filter by alliance ID, default: false" (cl-alliances (cons (parse-alliance :id str) (cl-alliances)))]
   [("-g" "--group") str "Select a groupid, default: false" (cl-groups (cons (group->id str) (cl-groups)))]
   #:once-each
   [("-d" "--date") str "Select start date, format: YYYYMMDD" (cl-date str)]
   [("-e" "--end-date") str "Select end date, format: YYYYMMDD" (cl-end str)]
   [("-l" "--link" "--href") "Show links to killmails, default: false" (cl-href #t)]
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
	      (string-join (map (lambda (n) (string-append "/groupID/" n)) groups) "/")
	      "")
	  (if (not (null? regions))
	      (string-append "/regionID/" (string-join regions))
	      "")
	  (if (not (null? (cl-end))) (string-append "/endTime/" (cl-end) "0000") "")
	  (if (not (null? alliances))
	      (string-append "/allianceID/" (string-join alliances ","))
	      ""))])
    (json-api built-url)))

(define (groupid->list lst)
  (append-map (lambda (i) (map (lambda (n) (vector-ref n 0)) (parse-type :members (string->number i)))) lst))

(define-syntax concat-data
  (syntax-rules (:alliance :shiptype :check)
    ((_ :alliance lst) (filter-map (lambda (x)
				     (cond
				      [(null? (cl-alliances)) x]
				      [(member (number->string (hash-ref x 'allianceID)) (cl-alliances)) x]
				      [else #f]))
				   lst))
    ((_ :shiptype lst) (filter-map (lambda (x)
				     (cond
				      [(null? (cl-groups)) x]
				      [(member (hash-ref x 'shipTypeID) (groupid->list (cl-groups))) x]
				      [else #f]))
				   lst))
    ((_ :check lst) (cond
		     [(null? (cl-alliances)) (concat-data :shiptype lst)]
		     [(null? (cl-groups)) (concat-data :alliance lst)]
		     [else (set-intersect (concat-data :alliance lst)
					  (concat-data :shiptype lst))]))))

(define-syntax parse-helper
  (syntax-rules ()
    ((_ hash location date id)
     (filter string?
	     (list
	      (parse-type :name (hash-ref hash 'shipTypeID))
	      (hash-ref hash 'characterName)
	      (hash-ref hash 'corporationName)
	      (hash-ref hash 'allianceName)
	      (parse-solarsystem :name location)
	      (parse-region :name (parse-solarsystem :region location))
	      date
	      (if (cl-href) (string-append "https://zkillboard.com/kill/" (number->string id) "/") #f))))))

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

;; HTML Output

(define (create-html-table lst)
  (if (empty? (cdr lst))
      #f
      (div 'class: "data"
	   (h2 (car lst))
	   (table
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
		  (cdr lst)))))))

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
       (literal (style/inline 'type: "text/css" "tr:nth-child(2n+1) > td { background-color: #efefef; }")))
      (body
       (div 'id: "content"
	    (h1 "EVE Killboard Digest")
	    (filter-map (lambda (t) (create-html-table t)) lst)))))))

;; Exec

;; (cl-alliances '("864733958"))
;; (cl-groups '("898"))
;; (cl-regions (list (id/string->string (parse-region :id "Fade"))))
(cl-date "20160120")

;; (define test (pull-url #:date (cl-date) #:alliances '("864733958") #:group (parse-group :id "Black Ops") #:kills #t #:losses #t))
(define test (pull-url))

(define test-kills (parse-kills test #:attackers #t))
(define test-losses (parse-kills test #:attackers #f))

(list
 (if (cl-kills) (parse-kills test #:attackers #t) null)
 (if (cl-losses) (parse-kills test #:attackers #f) null))

(with-output-to-file "/dev/shm/eve-digest.html"
  (lambda () (output-html (list (cons "Kills" test-kills) (cons "Losses" null))))
  #:exists 'truncate/replace)
