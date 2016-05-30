#lang racket

(require db)
(require db/util/datetime)

(require "eve-sql_main.rkt")

(provide (all-from-out db/util/datetime)
	 (all-defined-out))

(define (timerboard-prepare-table)
  (if (table-exists? sqlc "customTimerboard")
      (query-exec sqlc "TRUNCATE TABLE customTimerboard")
      (query-exec sqlc "CREATE TABLE customTimerboard ( allianceName VARCHAR(255) NOT NULL, type VARCHAR(50) NOT NULL, system VARCHAR(255) NOT NULL, constellation VARCHAR(255), region VARCHAR(255), datetime DATETIME )")))

(define (timerboard-replace lst)
  (for-each (lambda (x)
	      (query sqlc "REPLACE INTO customTimerboard VALUES (?, ?, ?, ?, ?, ?)"
		     (first x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)))
	    lst))

(define (timerboard-query)
  (map vector->list (query-rows sqlc "SELECT * FROM customTimerboard ORDER BY datetime")))

(define (timerboard-query-region query)
  (map vector->list (query-rows sqlc "SELECT * FROM customTimerboard WHERE region = ? ORDER BY datetime" query)))