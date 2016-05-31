#! /usr/bin/env racket
#lang racket

(require eve)

;; Read from stdin

(define pipe-input (let ([input (second (read))])
		     (cond [(empty? input) (exit 0)]
			   [else (cdr (append* input))])))

;; Parse data into sql ready format

(define (parse-tower-data lst)
  (map (lambda (km) (let ([location (parse-map (list-ref km 7))])
		      (flatten
		       (list
			(list-ref km 9)
			(vector-ref location 3)
			(cdr (split-moon-display (vector-ref location 6)))
			(list-ref km 3)
			(list-ref km 5)
			(list-ref km 10)
			(list-ref km 0)
			(list-ref km 11)))))
       lst))

;; Exec

(sql-tower-update-raw (parse-tower-data pipe-input))
