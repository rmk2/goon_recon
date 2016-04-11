#! /usr/bin/env racket
#lang racket

(require eve)

;; XML options

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; Limit the amount of polled data in memory to avoid running out of virtual
;; memory, especially on EDIS

(define (exec-limit-iteration #:input lst
			      #:digest [digest null]
			      #:limit [limit 4500])
  (let loop ([data (split-list lst limit)] [i 0] [result '()])
    (if (< i (length data))
	(loop data (+ i 1) (digest (exec-limit-api-rate #:function hash-poll-affiliation
							#:input (list-ref data i)
							#:digest map-hash-parse-affiliation
							#:delay 15
							#:limit 2500)))
	(exit 0))))

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

(exec-limit-iteration #:input (map number->string (sql-super-get-characterids))
		      #:digest sql-super-update-affiliations
		      #:limit 2500)
