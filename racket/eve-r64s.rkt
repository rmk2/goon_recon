#! /usr/bin/env racket
#lang racket

(require eve)

;; Read from stdin

(define input (cdr (append* (second (read)))))

;; Ony show us (known) money moons, append their type to the killmail data

(define (query-money-moons [input-list input])
  (filter-map (lambda (x)
		(let ([moon (vector-ref (parse-moon (list-ref x 7)) 0)])
		  (if (money-moon? moon)
		      (flatten (append x (last (vector->list (parse-moondata (split-moon-display moon))))))
		      #f)))
	      input-list))

;; Filter uniques, keep the newer result

(define (filter-unique lst)
  (reverse
   (remove-duplicates (reverse lst)
		      #:key (lambda (x) (eighth x)))))

;; Parse typeIDs

(define (parse-money-moons input-list)
  (map (lambda (kill) (list
		       (parse-type :name (list-ref kill 0))
		       (list-ref kill 4)
		       (list-ref kill 6)
		       (simplify-moon-display (parse-moon :name (list-ref kill 7)))
		       (parse-region :name (list-ref kill 9))
		       (list-ref kill 10)
		       (last kill)
		       (string-append "https://zkillboard.com/kill/" (number->string (list-ref kill 11)))))
       input-list))

;; Exec

(for-each (lambda (tower) (displayln (string-join tower ",")))
	  (parse-money-moons (filter-unique (query-money-moons))))
