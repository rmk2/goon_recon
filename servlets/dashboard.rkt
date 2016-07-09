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
	 "dashboard/moon-database.rkt"
	 "dashboard/report.rkt"
	 "dashboard/result.rkt"
	 "dashboard/tasks.rkt"
	 "dashboard/timers.rkt")

;; URL dispatch

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("recon" "report") exec-report]
   [("recon" "result") #:method "post" exec-result]
   [("recon" "moon-database") exec-moon-database]
   [("recon" "tasks") exec-tasks]
   [("recon" "timers") exec-timers]
   [("timers") exec-timers]))

(define (main req)
  (main-dispatch req))

;; Parameters

(define webroot (if (string? (getenv "EVEROOT")) (getenv "EVEROOT") "./"))

;; Mimetype table

(define system-mime-types (read-mime-types "/etc/mime.types"))

;; Servlet

(serve/servlet main
	       #:stateless? #t
	       #:port 8000
	       #:command-line? #t
	       #:banner? #t
	       #:servlet-regexp #rx""
	       #:servlet-current-directory webroot
	       #:mime-types-path (build-path "/etc/mime.types"))
