#!/usr/bin/env racket
;; #lang web-server
#lang racket

(require web-server/servlet
         web-server/servlet-env
	 web-server/private/mime-types)

(require net/uri-codec)

(require eve)

;; Include servlet component

(require "dashboard/common.rkt"
	 "dashboard/dscan.rkt"
	 "dashboard/goo-database.rkt"
	 "dashboard/moon-database.rkt"
	 "dashboard/parse.rkt"
	 "dashboard/report.rkt"
	 "dashboard/result.rkt"
	 "dashboard/tasks.rkt"
	 "dashboard/timers.rkt")

;; URL dispatch

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("recon" "report") exec-report]
   [("recon" "result") #:method "post" (lambda (req) (exec-result req #:persist-dscan (cl-persist)))]
   [("recon" "goo-database") exec-goo-database]
   [("recon" "moon-database") exec-moon-database]
   [("recon" "tasks") exec-tasks]
   [("recon" "timers") exec-timers]
   [("dscan" "intel") exec-dscan-report]
   [("dscan" "submit") #:method "post" (lambda (req) (exec-parse-dscan req #:persist-dscan (cl-persist)))]
   [("dscan" (string-arg)) exec-parse-archive]
   [("timers") exec-timers]))

(define (main req)
  (main-dispatch req))

;; Parameters

(define cl-port (make-parameter 8000))
(define cl-prefix (make-parameter null))
(define cl-persist (make-parameter #f))

;; Command-line argument handling

(define parse-args
  (command-line
   #:once-each
   [("-d" "--persist-dscan" "--save-dscan") "Save raw dscans to disk, default: false"
    (cl-persist #t)]
   [("-P" "--port") port "Use specified port, default: 8000"
    (if (number? (string->number port)) (cl-port (string->number port)) (cl-port))]
   [("-w" "--webroot" "-p" "--prefix") dir "Specify webroot directory, alternative: env EVEROOT=dir, default: current dir"
    (if (directory-exists? dir) (cl-prefix dir) (raise-user-error "[error] Directory does not exist:" dir))]))

;; Set webroot (used for storing files etc.)

(define webroot
  (cond [(not (null? (cl-prefix))) (cl-prefix)]
	[(string? (getenv "EVEROOT")) (getenv "EVEROOT")]
	[else "./"]))

;; Mimetype table

(define system-mime-types (read-mime-types "/etc/mime.types"))

;; Servlet

(serve/servlet main
	       #:stateless? #t
	       #:port (cl-port)
	       #:command-line? #t
	       #:banner? #t
	       #:servlet-regexp #rx""
	       #:servlet-current-directory webroot
	       #:mime-types-path (build-path "/etc/mime.types"))
