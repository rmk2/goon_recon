#! /usr/bin/env racket
#lang racket

(require eve)

;; Read from stdin

(define pipe-input (let ([input (second (read))])
		     (cond [(empty? input) (exit 0)]
			   [else (filter sql-killmail? (car input))])))

;; Parse data into sql ready format

(define (parse-tower-data lst)
  (filter-map (lambda (km) (if (positive? (sql-killmail-location km))
			       (let ([location (sql-parse->struct (parse-map (sql-killmail-location km))
								  #:struct mapDenormalize)])
				 (flatten
				  (list
				   (sql-killmail-region km)
				   (mapDenormalize-system location)
				   (cdr (split-moon-display (mapDenormalize-name location)))
				   (sql-killmail-corporationid km)
				   (sql-killmail-allianceid km)
				   (sql-killmail-datetime km)
				   (sql-killmail-shiptype km)
				   (sql-killmail-killid km))))
			       #f))
	      lst))

;; Exec

(sql-tower-update-raw (parse-tower-data pipe-input))
