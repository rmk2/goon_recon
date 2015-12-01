#! /usr/bin/env racket
#lang racket

(require json)
(require 2htdp/batch-io)
(require racket/string)
(require srfi/19)
(require net/url)

;; FILE HANDLING

(define api
  (let ([url "https://zkillboard.com/api/no-items/groupID/365/regionID/10000051/startTime/201511190000"])
    (call/input-url (string->url url)
		    get-pure-port
		    read-json)))

(define tower-typeids
  (let ([url "https://public-crest.eveonline.com/inventory/groups/365/"])
    (call/input-url (string->url url)
		    get-pure-port
		    read-json)))

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
    ((_ str) (car (regexp-match #px"[0-9]{4,5}" str)))))

(define typeid-parse
  (map (lambda (hash) (cons
		       (string->number (filter-id (hash-ref hash 'href)))
		       (hash-ref hash 'name)))
       (hash-ref tower-typeids 'types)))

(define-syntax convert-typeids
  (syntax-rules ()
    ((_ n) (if (assoc n typeid-parse)
	       (cdr (assoc n typeid-parse))
	       #f))))

(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

(define-syntax input-map-join
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-join x ",")) input))))

(define (zkill-towers)
  (let ([parse-data api])
    (filter-map (lambda (km-list)
		  (let ([victim (hash-ref km-list 'victim)]
			[date (hash-ref km-list 'killTime)]
			[location (hash-ref km-list 'solarSystemID)]
			[attackers (hash-ref km-list 'attackers)]
			[id (hash-ref km-list 'killID)])
		    (if (convert-typeids (hash-ref victim 'shipTypeID))
			(list
			 (convert-typeids (hash-ref victim 'shipTypeID))
			 (hash-ref victim 'corporationName)
			 (hash-ref victim 'allianceName)
			 (solar-parse :system (number->string location))
			 (solar-parse :region (number->string location))
			 date
			 (string-append "https://zkillboard.com/kill/" (number->string id) "/"))
			#f)))
		parse-data)))

(for-each (lambda (x) (displayln x))
	  (input-map-join (zkill-towers)))

