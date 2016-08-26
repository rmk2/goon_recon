#!/usr/bin/env racket
#lang racket

(require eve)

(require racket/file)

;; Parameters

(define cl-cron (make-parameter #f))
(define cl-limit (make-parameter 60))
(define cl-prefix (make-parameter (current-directory)))

;; Command-line argument handling

(define parse-args
  (command-line
   #:once-each
   [("-c" "--cron" "-q" "--quiet") "Suppress any non-error output, default: false"
    (cl-cron #t)]
   [("-l" "--limit" "-d" "--days") int "Delete files older than int days, default: 60"
    (if (exact-positive-integer? (string->number int))
	(cl-limit (string->number int))
	(raise-user-error "[error] Value is not a positive integer:" int))]
   [("-p" "--prefix") dir "Specify dscan directory, default: current dir"
    (if (directory-exists? dir)
	(cl-prefix dir)
	(raise-user-error "[error] Directory does not exist:" dir))]))

;; Get dscan filenames for prefix

(define (dscan-get-filenames [prefix (cl-prefix)])
  (map (lambda (path) (path->string (last (explode-path path))))
       (find-files (lambda (file) (regexp-match? #px".gz$" file)) prefix)))

;; Only show files older than (cl-limit) days AND not linked in database

(define (dscan-list-candidates [prefix (cl-prefix)] [limit (cl-limit)])
  ;; Get dscans linked in database
  (define (sql-dscan-get-scanids)
    (map vector->values
	 (foldl list*
		(query-rows sqlc "SELECT scanID FROM moonScanRaw WHERE scanID IS NOT NULL")
		(query-rows sqlc "SELECT scanID FROM citadelScanRaw WHERE scanID IS NOT NULL"))))
  ;; Get dscan paths for prefix
  (define (dscan-get-paths prefix)
    (find-files (lambda (file) (regexp-match? #px".gz$" file)) prefix))
  ;; Only show files older than (cl-limit) days (default: 60 days = 5.184e6 seconds)
  (define (dscan-filter-by-age prefix limit)
    (filter-map (lambda (path) (if (> (file-or-directory-modify-seconds path)
				      (- (current-seconds) (* 60 60 24 limit)))
				   path
				   #f))
		(dscan-get-paths prefix)))
  
  (set-subtract
   (dscan-get-paths prefix)
   (map (lambda (id) (build-path prefix (dscan-id->filename id))) (sql-dscan-get-scanids))
   (dscan-filter-by-age prefix limit)))

;; Delete candidates

(define (dscan-delete-obsolete [prefix (cl-prefix)] [limit (cl-limit)])
  (for-each (lambda (dscan) (if (void? (delete-file dscan))
				void
				(raise-user-error "[error] Unable to delete file: " dscan)))
	    (dscan-list-candidates prefix limit)))

(define (dscan-delete-obsolete-loop [prefix (cl-prefix)] [limit (cl-limit)])
  (let loop ([input (dscan-list-candidates prefix limit)] [i 0] [result null])
    (cond [(zero? (length input))
	   (printf "[success] No obsolete dscans found in ~a~%" prefix)]
	  [(< i (length input))
	   (loop input
		 (+ i 1)
		 (if (void? (delete-file (list-ref input i)))
		     (cons (list-ref input i) result)
		     (raise-user-error "[error] Unable to delete file:" dscan)))]
	  [else
	   (printf "[success] Deleted ~a obsolete dscans from ~a~%" (length result) prefix)])))

;; Exec

(if (cl-cron)
    (dscan-delete-obsolete (cl-prefix) (cl-limit))
    (dscan-delete-obsolete-loop (cl-prefix) (cl-limit)))

