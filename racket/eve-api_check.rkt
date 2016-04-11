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

(let ([input (map number->string (sql-super-get-characterids))] [limit 4000])
  (let loop ([data (filter (lambda (x) (not (empty? x))) (split-list input limit))] [i 0])
    (if (and (< i (length data)) (not (null? (list-ref data i))))
	(begin
	  (log-debug (format "[debug] Current iteration (~s items each): ~s of ~s" limit (+ i 1) (length data)))
	  (poll-api-helper (list-ref data i))
	  (loop data [+ i 1]))
	(exit 0))))
