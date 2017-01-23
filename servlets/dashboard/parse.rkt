#lang racket

(require eve)

(require "common.rkt")
(require "dscan.rkt")

(require racket/future)

(provide (all-defined-out))

;; Read dscan, write it to disk, redirect to id, which sends execution on to
;; (exec-parse-archive), which will then read the previously written dscan
;; from disk before passing it to (exec-parse). This is the easiest way to end
;; on a url already containing the dscan's id, which can then be shared or
;; called again later, bypassing the initial write stage and jumping directly
;; to (exec-parse-archive).

(define (exec-parse-dscan req #:persist-dscan persist-dscan?)

  (define-values (dscan)
    (values
     (extract-post-data req #"dscan")))

  (cond [(> (length (string-split dscan "\t")) 1)
	 (begin
	   (when persist-dscan? (dscan-local->string :write dscan))
	   (send/back (redirect-to (string-append "/dscan/" (dscan-local->string :id dscan)))))]
	[(and persist-dscan? (> (length (string-split dscan "\r\n")) 1))
	 (exec-write-local #:dscan dscan)]
	[else (exec-dscan #:dscan "No D-Scan found!" #:location null #:request req)]))

;; Read dscan from disk, send it on to get parsed

(define (exec-parse-archive req id)
  (if (file-exists? (build-path (dscan-id->filename id)))
      (let ([data (dscan-local->string :read-id id)])
	(if (list? data)
	    (exec-prepare-local #:dscan data #:request req)
	    (exec-prepare-dscan #:dscan data #:request req)))
      (exec-dscan #:dscan "No D-Scan found!" #:location null #:request req)))

;; Parse dscan, send it on to display its output

(define (exec-prepare-dscan #:dscan dscan #:request [req null])

  (define data-full
    (dscan-list->hash
     (dscan-raw->list dscan)))

  (define data-normalised
    (dscan-sort
     (dscan-list->hash
      (dscan-normalise-distance
       (dscan-raw->list dscan)))))

  (define data-ongrid
    (filter (lambda (hash) (< (hash-ref hash 'distance) (* 3 (max_distance))))
	    data-normalised))

  (define dscan-data
    (call-with-values
	(lambda()
	  (values
	   (filter-dscan :ship data-full)
	   (list
	    (filter-dscan :deployable data-full)
	    (filter-dscan :drone data-full)
	    (filter-dscan :fighter data-full))
	   (list
	    (filter-dscan :structure data-full)
	    (filter-dscan :sovereignty data-full))
	   (list
	    (filter-dscan :starbase data-ongrid))))
      output:create-html-dscan-rows))

  (define location (guess->location (dscan-guess-location data-normalised)))

  (exec-dscan #:dscan dscan-data #:location location #:request req))

;; Parse local scan, write it to disk, redirect to dscan/<id>

(define (exec-write-local #:dscan dscan)

  (chunk-size 120)
  (define time-diff (make-parameter (* 6 3600)))

  (define (poll-affiliation-helper lst)
    (exec-limit-api-rate #:function (lambda (a) (hash-poll-affiliation (map (lambda (x) (hash-ref x 'characterID)) a)))
			 #:input lst
			 #:delay 1
			 #:digest map-character-hash->struct
			 #:limit 1500))

  (define local-result
    (future
     (lambda ()
       (let ([input (map-hash-parse-unknown (string-split dscan "\r\n"))])
	 (append (car input)
		 (if (empty? (cadr input))
		     null
		     (poll-affiliation-helper (cadr input))))))))

  (sql-character-update-ids (touch local-result)) ;; Update SQL character data

  (dscan-local->string :write (touch local-result)) ;; Write local scan to disk
  
  (send/back (redirect-to (string-append "/dscan/" (dscan-local->string :id (touch local-result))))))

;; Parse local, send it on to display its output

(define (exec-prepare-local #:dscan dscan #:request [req null])

  (define dscan-data
    (output:create-html-dscan-rows
     #:local-scan #t
     (cons
      (count-affiliations (map sql-character-corporation dscan))
      (count-affiliations (map sql-character-alliance dscan)))
     (list (list
	    (list "Total" (length (map sql-character-name dscan))))
	   (list
	    (list "...in PC alliances" (length (filter-not string-empty? (map sql-character-alliance dscan))))
	    (list "...in NPC alliances" (length (filter string-empty? (map sql-character-alliance dscan)))))
	   (list
	    (list "Alliances" (length (remove-duplicates (filter-not string-empty? (map sql-character-alliance dscan)))))
	    (list "Corporations" (length (remove-duplicates (map sql-character-corporation dscan))))))
     null
     null))

  (exec-dscan #:dscan dscan-data #:location null #:request req))
