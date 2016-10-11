#! /usr/bin/env racket
#lang racket

(require eve)

;; Read from stdin

(define pipe-input (let ([input (second (read))])
		     (cond [(empty? input) (exit 0)]
			   [else (filter sql-killmail? (car input))])))

;; Filter invalid killmails

(define (parse-citadel-data lst)
  (filter (lambda (x) (positive? (sql-killmail-location x))) lst))

;; Exec

(sql-citadel-update-kill (parse-citadel-data pipe-input))
