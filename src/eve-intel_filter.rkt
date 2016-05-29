#! /usr/bin/env racket
#lang racket

(require eve)

;; Command-line argument handling

(define cl-filter (make-parameter null))
(define cl-csv (make-parameter #f))
(define cl-raw (make-parameter #f))
(define cl-case (make-parameter #f))
(define cl-name (make-parameter #f))

(define parse-args
  (command-line
   #:multi
   [("-e" "--regexp" "-f" "--filter") str "Custom filter" (cl-filter (cons  str (cl-filter)))]
   #:once-each
   [("-i" "--ignore-case") "Case insensitive search" (cl-case #t)]
   [("-n" "--unique-name") "Filter by unique name only" (cl-name #t)]
   #:once-any
   [("-p" "--print" "-c" "--csv") "Print output as csv strings, one entry per line" (cl-csv #t)]
   [("-l" "--list") "Print output as a Scheme list-of-strings, one sub-list per line" (cl-csv #f)]
   [("-r" "--raw") "Print output as a raw Scheme list" (cl-raw #t)]))

;; Data fetching

(define data (edis-data))

;; Macros

(define-syntax filter-results
  (syntax-rules ()
    ((_ filter) (filter-results filter (input-map-split data)))
    ((_ filter lst) (filter-map
		     (lambda (l)
		       (if (memf (lambda (x) (regexp-match (if (cl-case) (string-append "(?i:" filter ")") filter) x)) l) l #f))
		     lst))))

(define-syntax filter-unique
  (syntax-rules (:print :list)
    ((_ :list query) (for-each (lambda (r) (writeln r)) (filter-unique query)))
    ((_ :print query) (for-each (lambda (r) (displayln (string-join r ","))) (filter-unique query)))
    ((_ query) (reverse
		(remove-duplicates (reverse query)
				   #:key (lambda (x) (if (cl-name)
							 (list-ref x 1)
							 (string-join (list-tail (reverse x) 3))))
				   regexp-match)))))

(define (run-filter query)
  (let loop ([query query] [i 0] [result null])
    (cond
     [(zero? i) (loop query (+ i 1) (filter-results (list-ref (cl-filter) i)))]
     [(< i (length (cl-filter))) (loop query (+ i 1) (filter-results (list-ref (cl-filter) i) result))]
     [else result])))

;; Main

(define (main)
  (if (or (null? (cl-filter)) (member ".*" (cl-filter)))
      (if (cl-csv)
	  (for-each (lambda (x) (displayln x)) data)
	  data)
      (if (cl-raw)
	  (filter-unique (run-filter (cl-filter)))
	  (if (cl-csv)
	      (filter-unique :print (run-filter (cl-filter)))
	      (filter-unique :list (run-filter (cl-filter)))))))

;; ;; Exec

(main)

