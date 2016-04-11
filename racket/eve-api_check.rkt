#! /usr/bin/env racket
#lang racket

(require eve)

;; XML options

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; Limit the amount of polled data in memory to avoid running out of virtual
;; memory, especially on EDIS

(define (poll-api-helper lst)
  (sql-super-update-affiliations
   (exec-limit-api-rate #:function hash-poll-affiliation
			#:input lst
			#:digest map-hash-parse-affiliation
			#:delay 15
			#:limit 2500)))

;; affiliation -> sql-ready list

(define (map-hash-parse-affiliation lst)
  (map (lambda (hash) (list
		       (hash-ref hash 'characterID)
		       (hash-ref hash 'characterName)
		       (hash-ref hash 'corporationID)
		       (hash-ref hash 'corporationName)
		       (hash-ref hash 'allianceID)
		       (hash-ref hash 'allianceName)))
       lst))

;; Exec

(sql-super-populate-affiliations)

(define main-limit (make-parameter 4000))
(define sql-charids (split-list (map number->string (sql-super-get-characterids)) (main-limit)))

(define (main input index)
  (if (< index (length sql-charids))
      (let ([data (list-ref input index)])
	(begin
	  (log-debug (format "[debug] Current iteration (main loop, ~s items each): ~s of ~s"
			     (main-limit)
			     (+ index 1)
			     (length sql-charids)))
;;	  (poll-api-helper data)
	  (main input (+ index 1))))
      (exit 0)))

(main sql-charids 0)
