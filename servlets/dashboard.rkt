#!/usr/bin/env racket
;; #lang web-server
#lang racket

(require web-server/servlet
         web-server/servlet-env
	 web-server/private/mime-types
	 web-server/http/basic-auth)

(require net/uri-codec)

(require eve)
(require "../lib/eve-auth_basic.rkt")

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
	 "dashboard/timers.rkt"
	 "dashboard/groups.rkt"
	 "dashboard/register.rkt")

;; Auth dispatch

(define-values (auth-dispatch auth-url)
  (dispatch-rules
   [("register") exec-register]
   [("register") #:method "post" exec-register-post]
   [else exec-auth]))

(define (exec-auth req)
  (cond [(and (pair? (request->basic-credentials req))
	      (let* ([user (car (request->basic-credentials req))]
		     [pass (cdr (request->basic-credentials req))]
		     [data (auth:sql-auth-get-user (bytes->string/utf-8 user))])
		(and (scrypt-hash? data)
		     (auth:scrypt-check-hash data pass #:length 32))))
	 (main-dispatch (auth-add-header req))]
	[else
	 (response
	  401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
	  (list
	   (make-basic-auth-header
	    (format "Basic Auth Test: ~a" (gensym))))
	  void)]))

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
   [("management" "groups") exec-groups]
   [("management" "groups") #:method "post" exec-groups-modify]))

;; Embed valid JSON X-Auth header in every request for local testing

(define (auth-add-header req)
  (let ([group (if (string-empty? (cl-group)) "recon-l" (cl-group))])
    (cond [(not (false? (cl-json)))
	   (struct-copy request req
			[headers/raw (append
				      (list (auth:create-authorization-header (auth:create-token #:subject group)))
				      (request-headers/raw req))])]
	  [(pair? (request->basic-credentials req))
	   (struct-copy request req
			[headers/raw (append
				      (list (auth:create-authorization-header
					     (auth:create-token
					      #:subject (auth:sql-auth-get-user-group
							 (bytes->string/utf-8
							  (car (request->basic-credentials req)))))))
				      (request-headers/raw req))])]
	  [else req])))

;; Intermediate steps, then dispatch

(define (main req)
  (if (cl-auth)
      (auth-dispatch req)
      (main-dispatch (auth-add-header req))))

;; Parameters

(define cl-auth (make-parameter #f))
(define cl-json (make-parameter #f))
(define cl-group (make-parameter ""))
(define cl-port (make-parameter 8000))
(define cl-prefix (make-parameter null))
(define cl-persist (make-parameter #f))

;; Command-line argument handling

(define parse-args
  (command-line
   #:once-each
   [("-a" "--auth" "--basic-auth") "Use basic auth, default: false"
    (cl-auth #t)]
   [("-j" "--json" "--json-auth" "--json-test") "Create valid JSON header for ALL requests, default: false"
    (cl-json #t)]
   [("-g" "--group" "--json-group") group "Specify JSON user group, default: none"
    (if (string? group) (cl-group group) (cl-group))]
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
