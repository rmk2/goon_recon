#! /usr/bin/env racket
#lang racket

(require eve)

;; XML options

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; SQL

(define (sql-local-get-characterids)
  (query-list sqlc "SELECT characterID FROM customCharacters"))

;; Limit the amount of polled data in memory to avoid running out of virtual
;; memory, especially on EDIS

(define (poll-api-helper lst)
  (sql-character-update-ids
   (exec-limit-api-rate #:function hash-poll-affiliation
			#:input lst
			#:digest map-character-hash->struct
			#:delay 5
			#:limit 2500)))

;; Exec

(define main-limit (make-parameter 4000))
(define sql-charids (split-list (map number->string (sql-local-get-characterids)) (main-limit)))

(define (main input index)
  (if (< index (length sql-charids))
      (let ([data (list-ref input index)])
	(begin
	  (log-debug (format "[debug] Current iteration (main loop, ~s items each): ~s of ~s"
			     (main-limit)
			     (+ index 1)
			     (length sql-charids)))
	  (poll-api-helper data)
	  (main input (+ index 1))))
      (exit 0)))

(main sql-charids 0)
