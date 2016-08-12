#! /usr/bin/env racket
#lang racket

(require eve)

;; Get latest killID from zkillboard

(define (zkill-get-lastest-id)
  (apply max (map (lambda (hash) (hash-ref hash 'killID))
		  (json-api "https://zkillboard.com/api/no-items/groupID/30,659/limit/5/"))))

;; Parameters

(define cl-start (make-parameter (sql-super-latest-killid)))
(define cl-limit (make-parameter (zkill-get-lastest-id)))

(define global-command "eve-digest.rkt")
(define global-params "--all --raw --sql -g 'Supercarrier' -g 'Titan'")

;; Main

(define (run-digest #:limit [limit (cl-limit)] #:start [start (cl-start)] #:delay [delay 10])
  (define (sql-super-latest-killid-mod)
    (query-value sqlc "SELECT MAX(killID) FROM intelSuperRaw WHERE killID <= ?" limit))
  (define (sql-super-latest-datetime-mod)
    (query-value sqlc "SELECT MAX(datetime) FROM intelSuperRaw WHERE killID <= ?" limit))
  (let* ([command global-command]
	 [params global-params]
	 [kill-id (number->string (sql-super-latest-killid-mod))]
	 [program (string-append command " " params " -i " kill-id)])
    (if (< (sql-super-latest-killid-mod) limit)
	(let ([run (system program)])
	  (if (not (false? run))
	      (begin
		(log-debug "[debug] Polling killmails")
		(printf "last killID: ~a | last datetime: ~a~%"
			(let ([killid (sql-super-latest-killid-mod)])
			  (if (number? killid) killid 0))
			(let ([datetime (sql-super-latest-datetime-mod)])	      
			  (if (sql-timestamp? datetime)
			      (date->string (sql-datetime->srfi-date datetime) "~1 ~3")
			      0)))
		(log-debug (format "[debug] Waiting before next iteration: ~s seconds" delay))
		(sleep delay)
		(run-digest #:limit limit #:start start #:delay delay))
	      (error "Something went wrong!")))
	(exit 0))))

;; Exec

(run-digest #:limit (cl-limit) #:start (cl-start))
