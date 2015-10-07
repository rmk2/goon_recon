#! /usr/bin/env racket
#lang racket

(require json)
(require 2htdp/batch-io)
(require racket/string)
(require srfi/19)
(require net/url)

(define query-supers (make-parameter #f))
(define query-titans (make-parameter #f))
(define query-location (make-parameter #f))
(define query-system (make-parameter #f))
(define query-region (make-parameter #f))
(define query-interactive (make-parameter #f))

(define parse-args
  (command-line
   #:once-each
   [("-t" "--titans") "Include titans in output" (query-titans #t)]
   [("-s" "--supers") "Include supercarriers in output" (query-supers #t)]
   [("-l" "--location") "Include location in output" (query-location #t)]
   [("-S" "--system") "Include system in output" (query-system #t)]
   [("-R" "--region") "Include region in output" (query-region #t)]
   [("-i" "--interactive") "Read from stdin" (query-interactive #t)]
   #:once-any
   [("-a" "--all") "Include both titans and supercarriers in output" (begin (query-titans #t) (query-supers #t))]))

;; FILE HANDLING

(define api-supers
  (let ((url-supers (string-append
		     "https://zkillboard.com/api/kills/groupID/659/no-items/startTime/"
		     (date->string (current-date) "~Y~m~d")
		     "0000")))
    (call/input-url (string->url url-supers)
		    get-pure-port
		    read-json)))

(define api-titans
  (let ((url-titans (string-append
		     "https://zkillboard.com/api/kills/groupID/30/no-items/startTime/"
		     (date->string (current-date) "~Y~m~d")
		     "0000")))
    (call/input-url (string->url url-titans)
		    get-pure-port
		    read-json)))

(define api
  (append api-supers api-titans))

;; PARSING

(define solar-list
  (make-hash (read-csv-file "/home/ryko/eve-solarsystemids")))

(define-syntax solar-parse
  (syntax-rules (:system :region)
    ((_ :system str) (car (hash-ref solar-list str)))
    ((_ :region str) (cadr (hash-ref solar-list str)))
    ((_ str) (string-join (hash-ref solar-list str) ","))))

(define supers (hash 3514 "Revenant"
		     22852 "Hel"
		     23913 "Nyx"
		     23917 "Wyvern"
		     23919 "Aeon"))

(define titans (hash 671 "Erebus"
		     3764 "Leviathan"
		     11567 "Avatar"
		     23773 "Ragnarok"))

(define typeids (make-hash (append (hash->list supers) (hash->list titans))))

(define-syntax convert-typeids
  (syntax-rules (:titans :supers :check :print)
    ((_ :titans n) (if (hash-has-key? titans n) #t #f))
    ((_ :supers n) (if (hash-has-key? supers n) #t #f))
    ((_ :check n) (if (hash-has-key? typeids n) #t #f))
    ((_ :print n) (when (hash-has-key? typeids n) (hash-ref typeids n)))))

;; (define (zkill-parse-interactive)
;;   (let ([data (if (query-interactive)
;; 		  (read-json)
;; 		  (read-json (open-input-file "/dev/shm/all-involved.txt")))])
;;     (zkill-parse data)))

(define (zkill-parse)
  (let ([zkill (if (query-interactive)
		   (read-json)
		   api)])
    (call/cc
     (lambda (return)
       (for-each
	(lambda (km-list)
	  (let ([location (hash-ref km-list 'solarSystemID)])
	    (for-each
	     (lambda (x)
	       (when (cond [(and (query-titans) (query-supers)) (convert-typeids :check (hash-ref x 'shipTypeID))]
			   [(query-titans) (convert-typeids :titans (hash-ref x 'shipTypeID))]
			   [(query-supers) (convert-typeids :supers (hash-ref x 'shipTypeID))])
		 ;;			   [else (return "Use --titans (-t), --supers (-s), --all (-a) or --raw (-r) to filter and display ships")])
		 (printf "~a,~a,~a,~a,~a~a~%"
			 (if (or (query-titans) (query-supers))
			     (convert-typeids :print (hash-ref x 'shipTypeID))
			     (hash-ref x 'shipTypeID))
			 (hash-ref x 'characterName)
			 (hash-ref x 'corporationName)
			 (hash-ref x 'allianceName)
			 (solar-parse (number->string location))
			 (if (query-interactive) "" (string-append "," (date->string (current-date) "~1"))))))
	     (hash-ref km-list 'attackers))))
	zkill)))))

(zkill-parse)

