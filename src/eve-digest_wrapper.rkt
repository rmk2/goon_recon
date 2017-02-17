#! /usr/bin/env racket
#lang racket

(require eve)

;; Get latest killID from zkillboard

(define (zkill-get-lastest-id [groups '("30" "659")])
  (apply max (map (lambda (hash) (hash-ref hash 'killID))
		  (json-api (format "https://zkillboard.com/api/zkbOnly/groupID/~a/limit/5/" (string-join groups ","))))))

;; Parameters

(define cl-groups (make-parameter '("30" "659")))
(define cl-limit (make-parameter 200))

(define cl-delay (make-parameter 10))

(define cl-start (make-parameter null))
(define cl-end (make-parameter (zkill-get-lastest-id (cl-groups))))

;; Command-line argument handling

(define parse-args
  (command-line
   #:once-each
   [("-d" "--delay") str "Delay between iterations in seconds, default: 10"
    (if (number? (string->number str)) (cl-delay str) (cl-delay))]
   [("-s" "--start" "-i" "--id") str "Select start killID, default: auto"
    (if (number? (string->number str)) (cl-start str) (cl-start))]
   [("-l" "--limit") str "Limit the amount of killmails per poll, default: 200, max: 200"
    (if (and (number? (string->number str)) (<= (string->number str) 200)) (cl-limit str) (cl-limit))]))

;; Main

(define (run-digest #:groups [groups (cl-groups)]
		    #:start [start (cl-start)]
		    #:end [end (cl-end)]
		    #:delay [delay (cl-delay)]
		    #:limit [limit (cl-limit)])
  (define (sql-super-latest-killid-mod)
    (query-maybe-value sqlc "SELECT MAX(killID) FROM intelSuperRaw WHERE killID <= ?" end))
  (define (sql-super-latest-datetime-mod)
    (query-maybe-value sqlc "SELECT MAX(datetime) FROM intelSuperRaw WHERE killID <= ?" end))
  (if (or (false? (sql-super-latest-killid-mod)) (< (sql-super-latest-killid-mod) end))
      (begin
	(log-debug "[debug] Polling killmails")
	(let* ([id (if (null? start) (number->string (sql-super-latest-killid-mod)) start)]
	       [data-raw (digest:poll-url #:date null #:groups groups #:kills #t #:losses #t #:id id #:limit limit)]
	       [kills (map (lambda (km) (begin (set-sql-killmail-eventtype! km "Kill") km))
			   (digest:parse-kills #:attackers #t #:raw #t #:groups groups data-raw))]
	       [losses (map (lambda (km) (begin (set-sql-killmail-eventtype! km "Loss") km))
			    (digest:parse-kills #:attackers #f #:raw #t #:groups groups data-raw))]
	       [data (append kills losses)])
	  (if (list? data)
	      (begin
		(sql-super-insert-killmails data)
		(sql-super-populate-affiliations)
		(printf "last killID: ~a | last datetime: ~a~%"
			(let ([killid (sql-super-latest-killid-mod)])
			  (if (number? killid) killid 0))
			(let ([datetime (sql-super-latest-datetime-mod)])
			  (if (sql-timestamp? datetime)
			      (date->string (sql-datetime->srfi-date datetime) "~1 ~3")
			      0)))
		(log-debug (format "[debug] Waiting before next iteration: ~s seconds" delay))
		(sleep delay)
		(run-digest #:groups groups #:start null #:end end #:delay delay #:limit limit))
	      (error "Something went wrong!"))))
      (exit 0)))

;; Exec

(run-digest #:groups (cl-groups) #:start (cl-start) #:end (cl-end) #:limit (cl-limit))
