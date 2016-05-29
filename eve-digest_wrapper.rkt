#! /usr/bin/env racket
#lang racket

(require eve)

(define cl-start (make-parameter 35611252))
(define cl-limit (make-parameter 43517856))

(define global-command "eve-digest.rkt")
(define global-params "--all --raw --sql -g 'Supercarrier' -g 'Titan'")

(define (run-digest #:limit [limit (cl-limit)] #:start [start (cl-start)])
  (define (sql-super-latest-killid-mod)
    (query-value sqlc "SELECT MAX(killID) FROM intelSuperRaw WHERE killID <= ?" (cl-limit)))
  (define (sql-super-latest-datetime-mod)
    (query-value sqlc "SELECT MAX(datetime) FROM intelSuperRaw WHERE killID <= ?" (cl-limit)))
  (let* ([command global-command]
	 [params global-params]
	 [kill-id (number->string (sql-super-latest-killid-mod))]
	 [program (string-append command " " params " -i " kill-id)])
    (if (< (sql-super-latest-killid-mod) limit)
	(let ([run (system program)])
	  (if (not (false? run))
	      (begin
		(printf "last killID: ~a | last datetime: ~a~%"
			(let ([killid (sql-super-latest-killid-mod)])
			  (if (number? killid) killid 0))
			(let ([datetime (sql-super-latest-datetime-mod)])	      
			  (if (sql-timestamp? datetime)
			      (date->string (sql-datetime->srfi-date datetime) "~1 ~3")
			      0)))
		(sleep 10)
		(run-digest))
	      (error "Something went wrong!")))
	(exit 0))))

;; Exec

(run-digest #:limit (cl-limit) #:start (cl-start))
