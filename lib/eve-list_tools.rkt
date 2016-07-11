#lang racket

(provide (all-defined-out))

;; String-split/join each list-of-strings within in a given list

(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

(define-syntax input-map-join
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-join x ",")) input))))

;; Filter duplicates with a list's car as key, preserving original order (desc)

(define (unique-car lst [f car])
  (reverse
   (remove-duplicates (reverse lst)
		      #:key (lambda (x) (string-downcase (f x)))
		      string=?)))

;; Split a list into members of length n

(define (split-list lst [n 100])
  (let loop ([query lst] [limit n] [i 1] [result null])
    (if (<= (* i limit) (length query))
	(loop query limit (+ i 1) (list* (drop (take query (* i limit)) (* (- i 1) limit)) result))
	(reverse (list* (take-right query (remainder (length query) limit)) result)))))

;; Count duplicates in a list, shortening the list until we hit length 0

(define (count-duplicates input)
  (let loop ([element (car input)] [counter 0] [lst input] [result null])
    (let ((first-element (unless (null? lst) (car lst))))
      (cond
       ((null? lst)
	(reverse (cons (list element counter) result)))
       ((equal? first-element element)
	(loop element (+ counter 1) (cdr lst) result))
       (else
	(loop first-element 1 (cdr lst) (cons (list element counter) result)))))))
