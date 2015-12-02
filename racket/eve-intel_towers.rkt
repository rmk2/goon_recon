#! /usr/bin/env racket
#lang racket

(require json)
(require 2htdp/batch-io)
(require srfi/19)
(require net/url)

;; Command-line argument handling

(define cl-regions (make-parameter null))
(define cl-date (make-parameter (date->string (current-date) "~Y~m~d")))
(define cl-end (make-parameter ""))
(define cl-href (make-parameter #f))
(define cl-attacker (make-parameter #f))
(define cl-group (make-parameter 365))
(define cl-losses (make-parameter #t))

(define parse-args
  (command-line
   #:multi
   [("-r" "--region") str "Select regions to use in the query" (cl-regions (cons str (cl-regions)))]
   #:once-each
   [("-d" "--date") str "Select start date, format: YYYYMMDD" (cl-date str)]
   [("-e" "--end-date") str "Select end date, format: YYYYMMDD" (cl-end str)]
   [("-l" "--link" "--href") "Show links to killmails, default: false" (cl-href #t)]
   [("-a" "--attackers") "Show a list of attacking alliances, default: false" (cl-attacker #t)]
   [("-g" "--group") str "Select a groupid, default: 365" (cl-group str)]
   [("-k" "--kills") "Show kills by <groupid>, default: false" (cl-losses #f)]))

;; DATA fetching

(define (json-api str)
  (call/input-url (string->url str) get-pure-port read-json))

(define typeids (json-api (string-append "https://public-crest.eveonline.com/inventory/groups/"
					 (if (number? (cl-group)) (number->string (cl-group)) (cl-group))
					 "/")))

(define region-ids (json-api "https://public-crest.eveonline.com/regions/"))

(define (pull-url [regionid 11000001] [date (cl-date)] [group (cl-group)])
  (when (number? regionid)
    (let ([built-url
	   (string-append "https://zkillboard.com/api/"
			  (if (cl-losses) "losses" "kills")
			  "/groupID/"
			  (if (number? group) (number->string group) group)
			  "/regionID/"
			  (if (number? regionid) (number->string regionid) regionid)
			  "/no-items/startTime/"
			  (if (number? date) (number->string date) date)
			  "0000"
			  (if (not (null? (cl-end))) (string-append "/endTime/" (cl-end) "0000") ""))])
      (json-api built-url))))

;; PARSING

(define solar-list
  (make-hash (read-csv-file "/home/ryko/eve-solarsystemids")))

(define-syntax solar-parse
  (syntax-rules (:system :region)
    ((_ :system str) (car (hash-ref solar-list str)))
    ((_ :region str) (cadr (hash-ref solar-list str)))
    ((_ str) (string-join (hash-ref solar-list str) ","))))

(define-syntax filter-id
  (syntax-rules ()
    ((_ str) (car (regexp-match #px"[0-9]{3,8}" str)))))

(define typeid-parse
  (map (lambda (hash) (cons
		       (string->number (filter-id (hash-ref hash 'href)))
		       (hash-ref hash 'name)))
       (hash-ref typeids 'types)))

(define regionid-parse
  (map (lambda (hash) (cons
		       (hash-ref hash 'name)
		       (string->number (filter-id (hash-ref hash 'href)))))
       (hash-ref region-ids 'items)))

(define-syntax convert-typeids
  (syntax-rules (:id :name)
    ((_ :id n) (if (assoc n typeid-parse)
		   (cdr (assoc n typeid-parse))
		   (number->string n)))
    ((_ :name str) (if (assoc str regionid-parse)
		       (cdr (assoc str regionid-parse))
		       #f))))

(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

(define-syntax input-map-join
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-join x ",")) input))))

(define-syntax string-empty?
  (syntax-rules ()
    ((_ str) (if (equal? "" str)
		 #t
		 #f))))

(define-syntax concat-attackers
  (syntax-rules ()
    ((_ hash) (remove-duplicates (map (lambda (x)
					(if (string-empty? (hash-ref x 'allianceName))
					    "none"
					    (hash-ref x 'allianceName)))
				      hash)))))

(define (zkill-parse url)
  (let ([parse-data url])
    (filter-map (lambda (km-list)
		  (let ([victim (hash-ref km-list 'victim)]
			[date (hash-ref km-list 'killTime)]
			[location (hash-ref km-list 'solarSystemID)]
			[attackers (hash-ref km-list 'attackers)]
			[id (hash-ref km-list 'killID)])
		    (filter string?
			    (list
			     (convert-typeids :id (hash-ref victim 'shipTypeID))
			     (if (string-empty? (hash-ref victim 'characterName)) #f (hash-ref victim 'characterName))
			     (hash-ref victim 'corporationName)
			     (hash-ref victim 'allianceName)
			     (solar-parse :system (number->string location))
			     (solar-parse :region (number->string location))
			     date
			     (if (cl-href) (string-append "https://zkillboard.com/kill/" (number->string id) "/") #f)
			     (if (cl-attacker) (string-join (concat-attackers attackers) "|") #f)))))
		parse-data)))

(define (run-regions lst)
  (let loop ([query lst] [i 0] [result null])
    (if (< i (length query))
	(loop query (+ i 1) (append result (zkill-parse (pull-url (convert-typeids :name (list-ref query i)) (cl-date) (cl-group)))))
	result)))

(for-each (lambda (x) (displayln x))
	  (input-map-join (run-regions (cl-regions))))
