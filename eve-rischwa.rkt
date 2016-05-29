#! /usr/bin/env racket
#lang racket

(require json)
(require racket/date)
(require net/url)

(define api
  (let [(file "/dev/shm/rischwa_coalitions.json")]
    (if (and (file-exists? file)
	     (> (file-or-directory-modify-seconds file) (- (current-seconds) 3600)))
	(read-json (open-input-file file))
	(call/input-url (string->url "http://rischwa.net/api/coalitions/current")
			get-pure-port
			read-json))))

(define-syntax json-filter
  (syntax-rules (:name :alliance)
    ((_ :name f) (hash-ref f 'name))))

(define (rischwa-query)
  (let ([query-data (hash-ref api 'coalitions)])
    (map (lambda (x) (list
		      (json-filter :name x)
		      (map (lambda (y) (json-filter :name y)) (hash-ref x 'alliances))))
	 query-data)))

(define (print-alliances)
  (let loop ([coalitions (rischwa-query)] [i 0] [result '()])
    (if (< i (length coalitions))
	(loop coalitions
	      (+ i 1)
	      (append (map (lambda (x) (string-append (car (list-ref coalitions i)) "," x)) (cadr (list-ref coalitions i))) result))
	(for-each (lambda (y) (displayln y)) (reverse result)))))

(print-alliances)
