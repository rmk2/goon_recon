#lang racket

(require eve)

(require net/uri-codec)

(require "common.rkt")
(require "dscan.rkt")

(provide (all-defined-out))

;; Read dscan, write it to disk, redirect to id, which sends execution on to
;; (exec-parse-archive), which will then read the previously written dscan
;; from disk before passing it to (exec-parse). This is the easiest way to end
;; on a url already containing the dscan's id, which can then be shared or
;; called again later, bypassing the initial write stage and jumping directly
;; to (exec-parse-archive).

(define (exec-parse-dscan req #:persist-dscan persist-dscan?)

  (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
  (define form-data (form-urlencoded->alist post-data))

  (define-values (dscan)
    (vector->values
     (list->vector
      (map cdr (form-urlencoded->alist post-data)))))

  (cond [(> (length (string-split dscan "\t")) 1)
	 (begin
	   (when persist-dscan? (dscan-gzip-write dscan))
	   (send/back (redirect-to (dscan-data->id dscan))))]
	[else (exec-dscan #:dscan "No D-Scan found!" #:location null)]))

;; Read dscan from disk, send it on to get parsed

(define (exec-parse-archive req id)
  (if (file-exists? (build-path (dscan-id->filename id)))
      (exec-parse #:dscan (bytes->string/locale (dscan-gunzip-read id)))
      (exec-dscan #:dscan "No D-Scan found!" #:location null)))

;; Parse dscan, send it on to display its output

(define (exec-parse #:dscan dscan)

  (define data-full
    (dscan-list->hash
     (dscan-raw->list dscan)))

  (define data-normalised
    (dscan-sort
     (dscan-list->hash
      (dscan-normalise-distance
       (dscan-raw->list dscan)))))

  (define data-ongrid
    (filter (lambda (hash) (< (hash-ref hash 'distance) (* 3 (max_distance))))
	    data-normalised))

  (define dscan-data
    (call-with-values
	(lambda()
	  (values
	   (filter-dscan :ship data-full)
	   (list
	    (filter-dscan :deployable data-full)
	    (filter-dscan :drone data-full)
	    (filter-dscan :fighter data-full))
	   (list
	    (filter-dscan :structure data-full)
	    (filter-dscan :sovereignty data-full))
	   (list
	    (filter-dscan :starbase data-ongrid))))
      output:create-html-dscan-rows))

  (define location (guess->location (dscan-guess-location data-normalised)))

  (exec-dscan #:dscan dscan-data #:location location))
