#! /usr/bin/env racket
#lang racket

(require net/url)

;; Command-line argument handling

(define cl-filter (make-parameter null))
(define cl-csv (make-parameter #f))
(define cl-raw (make-parameter #f))

(define parse-args
  (command-line
   #:multi
   [("-e" "--regexp" "-f" "--filter") str "Custom filter" (cl-filter (cons  str (cl-filter)))]
   #:once-any
   [("-p" "--print" "-c" "--csv") "Print output as csv strings, one entry per line" (cl-csv #t)]
   [("-l" "--list") "Print output as a Scheme list-of-strings, one sub-list per line" (cl-csv #f)]
   [("-r" "--raw") "Print output as a raw Scheme list" (cl-raw #t)]))

;; Data fetching

(define (unify-data)
  (let ([collected-file "/var/www/servers/eve.rmk2.org/pages/eve-intel_retroactive.txt"]
	[regions-file "/var/www/servers/eve.rmk2.org/pages/eve-intel_regions.txt"])
    (if (and (file-exists? collected-file) (file-exists? regions-file))
	(append (file->lines collected-file) (file->lines regions-file))
	(let ([collected "https://eve.rmk2.org/eve-intel_retroactive.txt"]
	      [regions "https://eve.rmk2.org/eve-intel_regions.txt"])
	  (append (call/input-url (string->url collected) get-pure-port port->lines)
		  (call/input-url (string->url regions) get-pure-port port->lines))))))

(define data (unify-data))

;; Macros

(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

;; (define-syntax input-map-join
;;   (syntax-rules ()
;;     ((_ input) (map (lambda (x) (string-join x ",")) input))))

(define-syntax filter-results
  (syntax-rules ()
    ((_ filter) (filter-results filter (input-map-split data)))
    ((_ filter lst) (filter-map
		     (lambda (l)
		       (if (memf (lambda (x) (regexp-match filter x)) l) l #f))
		     lst))))

(define-syntax filter-unique
  (syntax-rules (:print :list)
    ((_ :list query) (for-each (lambda (r) (writeln r)) (filter-unique query)))
    ((_ :print query) (for-each (lambda (r) (displayln (string-join r ","))) (filter-unique query)))
    ((_ query) (reverse
		(remove-duplicates (reverse query)
				   #:key (lambda (x) (string-join (list-tail (reverse x) 3)))
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
