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

(define parse-args
  (command-line
   #:multi
   [("-r" "--region") str "Select regions to use in the query" (cl-regions (cons str (cl-regions)))]
   #:once-each
   [("-d" "--date") str "Select start date, format: YYYYMMDD" (cl-date str)]
   [("-e" "--end-date") str "Select end date, format: YYYYMMDD" (cl-end str)]))

;; Data fetching

(define (json-api str)
  (call/input-url (string->url str) get-pure-port read-json))

(define api (json-api "https://zkillboard.com/api/no-items/groupID/365/regionID/10000051/startTime/201511190000"))

(define tower-typeids (json-api "https://public-crest.eveonline.com/inventory/groups/365/"))

(define region-ids (json-api "https://public-crest.eveonline.com/regions/"))

(define (pull-url [regionid 11000001] [date (cl-date)])
  (when (number? regionid)
    (let ([built-url
	   (string-append "https://zkillboard.com/api/groupID/365/regionID/"
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
    ((_ str) (car (regexp-match #px"[0-9]{4,8}" str)))))

(define typeid-parse
  (map (lambda (hash) (cons
		       (string->number (filter-id (hash-ref hash 'href)))
		       (hash-ref hash 'name)))
       (hash-ref tower-typeids 'types)))

(define regionid-parse
  (map (lambda (hash) (cons
		       (hash-ref hash 'name)
		       (string->number (filter-id (hash-ref hash 'href)))))
       (hash-ref region-ids 'items)))

(define-syntax convert-typeids
  (syntax-rules (:id :name)
    ((_ :id n) (if (assoc n typeid-parse)
		   (cdr (assoc n typeid-parse))
		   #f))
    ((_ :name str) (if (assoc str regionid-parse)
		       (cdr (assoc str regionid-parse))
		       #f))))

(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

(define-syntax input-map-join
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-join x ",")) input))))

(define (run-regions lst)
  (let loop ([query lst] [i 0] [result null])
    (if (< i (length query))
	(loop query (+ i 1) (append result (zkill-towers (pull-url (convert-typeids :name (list-ref query i)) (cl-date)))))
	result)))

(define (zkill-towers url)
  (let ([parse-data url])
    (filter-map (lambda (km-list)
		  (let ([victim (hash-ref km-list 'victim)]
			[date (hash-ref km-list 'killTime)]
			[location (hash-ref km-list 'solarSystemID)]
			[attackers (hash-ref km-list 'attackers)]
			[id (hash-ref km-list 'killID)])
		    (if (convert-typeids :id (hash-ref victim 'shipTypeID))
			(list
			 (convert-typeids :id (hash-ref victim 'shipTypeID))
			 (hash-ref victim 'corporationName)
			 (hash-ref victim 'allianceName)
			 (solar-parse :system (number->string location))
			 (solar-parse :region (number->string location))
			 date
			 (string-append "https://zkillboard.com/kill/" (number->string id) "/"))
			#f)))
		parse-data)))

(for-each (lambda (x) (displayln x))
	  (input-map-join (run-regions (cl-regions))))
