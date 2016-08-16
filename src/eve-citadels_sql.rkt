#! /usr/bin/env racket
#lang racket

(require eve)

;; Read from stdin

(define pipe-input (let ([input (second (read))])
		     (cond [(empty? input) (exit 0)]
			   [else (filter sql-killmail? (car input))])))

;; Parse data into sql ready format

(define (parse-citadel-data lst)
  (filter-map (lambda (km) (if (positive? (sql-killmail-location km))
			       (flatten
				(list
				 (sql-killmail-region km)
				 (sql-killmail-system km)
				 (sql-killmail-location km)
				 (sql-killmail-corporationid km)
				 (sql-killmail-allianceid km)
				 (sql-killmail-datetime km)
				 (sql-killmail-shiptype km)
				 (sql-killmail-killid km)))
			       #f))
	      lst))

;; Exec

(sql-citadel-update-kill (parse-citadel-data pipe-input))
