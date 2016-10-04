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
	 "dashboard/citadel-database.rkt"
	 "dashboard/dscan.rkt"
	 "dashboard/goo-database.rkt"
	 "dashboard/input-corporation.rkt"
	 "dashboard/moon-database.rkt"
	 "dashboard/parse.rkt"
	 "dashboard/report.rkt"
	 "dashboard/result.rkt"
	 "dashboard/tasks.rkt"
	 "dashboard/timers.rkt")

;; URL dispatch

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("report") exec-report]
   [("report") #:method "post" (lambda (req) (exec-result req #:persist-dscan (cl-persist)))]
   [("citadel-database") exec-citadel-database]
   [("citadel-database") #:method "post" exec-citadel-database-delete]
   [("goo-database") exec-goo-database]
   [("moon-database") exec-moon-database]
   [("tasks") exec-tasks]
   [("timers") exec-timers]
   [("input" "corporation" (integer-arg)) exec-input-corporation]
   [("input" "corporation" (string-arg)) exec-input-corporation]
   [("dscan") exec-dscan-report]
   [("dscan") #:method "post" (lambda (req) (exec-parse-dscan req #:persist-dscan (cl-persist)))]
   [("dscan" "intel") (send/back (redirect-to "/dscan" permanently))]
   [("dscan" (string-arg)) exec-parse-archive]
   [("timers") exec-timers]))

;; Revive SQL connection if it disconnected, then dispatch

(define (main req)
  (sql-revive-connection)
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
