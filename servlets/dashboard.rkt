#!/usr/bin/env racket
;; #lang web-server
#lang racket

(require web-server/servlet
         web-server/servlet-env
	 web-server/private/mime-types
	 web-server/http/basic-auth)

(require net/uri-codec)
(require (only-in racket/date
		  date->seconds))

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
	 "dashboard/timers.rkt"
	 "dashboard/groups.rkt"
	 "dashboard/login.rkt"
	 "dashboard/register.rkt")

;; Login dispatch

(define (login-add-referer req)
  (struct-copy request req
	       [headers/raw (append
			     (list (make-header
				    #"X-Referer"
				    (string->bytes/utf-8 (url->string (request-uri req)))))
			     (request-headers/raw req))]))

(define-values (login-dispatch login-url)
  (dispatch-rules
   [("dscan" (string-arg)) exec-parse-archive]
   [("register") exec-register]
   [("register") #:method "post" exec-register-post]
   [("login") exec-login]
   [((string-arg)) #:method "post" exec-login-post]
   [else (lambda (req) (exec-login (login-add-referer req)))]))

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
	 (cond [(cl-group) (group-dispatch (auth-add-header req))]
	       [else (admin-dispatch (auth-add-header req))])]
	[else
	 (response
	  401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
	  (list
	   (make-basic-auth-header
	    (format "Basic Auth Test: ~a" (gensym))))
	  void)]))

;; URL dispatch

(define-values (admin-dispatch admin-url)
  (dispatch-rules
   [("management" "groups") exec-groups]
   [("management" "groups") #:method "post" exec-groups-modify]
   [else recon-l-dispatch]))

(define-values (recon-l-dispatch recon-l-url)
  (dispatch-rules
   [("citadel-database") exec-citadel-database]
   [("citadel-database") #:method "post" exec-citadel-database-delete]
   [("goo-database") exec-goo-database]
   [("moon-database") exec-moon-database]
   [("input" "corporation" (integer-arg)) exec-input-corporation]
   [("input" "corporation" (string-arg)) exec-input-corporation]
   [else recon-dispatch]))

(define-values (recon-dispatch recon-url)
  (dispatch-rules
   [("tasks") exec-tasks]
   [("timers") exec-timers]
   [else affiliate-dispatch]))

(define-values (affiliate-dispatch affiliate-url)
  (dispatch-rules
   [("report") exec-report]
   [("report") #:method "post" (lambda (req) (exec-result req #:persist-dscan (cl-persist)))]
   [else public-dispatch]))

(define-values (public-dispatch public-url)
  (dispatch-rules
   [("dscan") exec-dscan-report]
   [("dscan") #:method "post" (lambda (req) (exec-parse-dscan req #:persist-dscan (cl-persist)))]
   [("dscan" "intel") (send/back (redirect-to "/dscan" permanently))]
   [("dscan" (string-arg)) exec-parse-archive]
   [("register") exec-register]
   [("register") #:method "post" exec-register-post]
   [("login") exec-login]
   [("login") #:method "post" exec-login-post]))

;; Embed valid JSON X-Auth header in every request for local testing or read
;; from basic auth groups

(define (auth-cookie? req)
  (findf (lambda (c) (string=? "access_token" (client-cookie-name c))) (request-cookies req)))

(define (auth-refresh-token req)
  (let* ([auth-cookie (auth-cookie? req)]
	 [auth-token (if (client-cookie? auth-cookie) (client-cookie-value auth-cookie) #f)]
	 [auth-struct (if (string? auth-token) (auth:extract-data (auth:verify-token auth-token)) #f)])
    (cond
     [(and (not (false? auth-struct))
	   (< (- (date->seconds (recon-jwt-expiration auth-struct)) (current-seconds)) 300))
      (let ([new-token (auth:create-token #:subject (recon-jwt-subject auth-struct)
					  #:username (recon-jwt-username auth-struct))])
	(redirect-to (url->string (request-uri req))
		     #:headers (list (auth:create-authorization-header new-token)
				     (cookie->header (make-cookie "access_token" new-token #:max-age 600)))))]
     [(cl-group) (group-dispatch (auth-add-header req))]
     [else (admin-dispatch (auth-add-header req))])))

(define (auth-add-header req)
  (let* ([group (if (string-empty? (cl-test)) "recon-l" (cl-test))]
	 [auth-cookie (auth-cookie? req)]
	 [auth-token (if (client-cookie? auth-cookie) (client-cookie-value auth-cookie) #f)]
	 [auth-struct (if (string? auth-token) (auth:extract-data (auth:verify-token auth-token)) #f)])
    (cond [(not (false? auth-struct))
	   (struct-copy request req
			[headers/raw (append
				      (list (auth:create-authorization-header auth-token))
				      (request-headers/raw req))])]
	  [(not (false? (cl-json)))
	   (struct-copy request req
			[headers/raw (append
				      (list (auth:create-authorization-header
					     (auth:create-token
					      #:subject group)))
				      (request-headers/raw req))])]
	  [(pair? (request->basic-credentials req))
	   (let ([user (bytes->string/utf-8 (car (request->basic-credentials req)))])
	     (struct-copy request req
			  [headers/raw (append
					(list (auth:create-authorization-header
					       (auth:create-token
						#:subject (auth:sql-auth-get-user-group :name user)
						#:username user)))
					(request-headers/raw req))]))]
	  [else req])))

;; Intermediate steps, then dispatch

(define (group-dispatch req)
  (let ([user-group (cond [(null? (auth:extract-authorization-header (request-headers/raw req)))
			   1]
			  [(string-empty? (auth:try-authorization-header :username req))
			   (auth:sql-auth-get-group-association :id (auth:try-authorization-header :subject req))]
			  [else
			   (auth:sql-auth-get-user-group :id (auth:try-authorization-header :username req))])])
    (cond [(>= user-group 256) (admin-dispatch req)]
	  [(>= user-group 64) (recon-l-dispatch req)]
	  [(>= user-group 32) (recon-dispatch req)]
	  [(>= user-group 4) (affiliate-dispatch req)]
	  [else (public-dispatch req)])))

(define (main req)
  (cond [(and (cl-login) (false? (auth-cookie? req))) (login-dispatch req)]
	[(cl-login) (auth-refresh-token req)]
	[(cl-auth) (auth-dispatch req)]
	[(cl-group) (group-dispatch (auth-add-header req))]
	[else (admin-dispatch (auth-add-header req))]))

;; Parameters

(define cl-auth (make-parameter #f))
(define cl-group (make-parameter #f))
(define cl-json (make-parameter #f))
(define cl-login (make-parameter #f))
(define cl-test (make-parameter ""))
(define cl-port (make-parameter 8000))
(define cl-prefix (make-parameter null))
(define cl-persist (make-parameter #f))

;; Command-line argument handling

(define parse-args
  (command-line
   #:once-each
   [("-g" "--group" "--group-auth") "Use basic group auth, default: false"
    (cl-group #t)]
   [("-j" "--json" "--json-auth" "--json-test") "Create valid JSON header for ALL requests, default: false"
    (cl-json #t)]
   [("-G" "--test-group" "--json-group") group "Specify JSON user group, default: none"
    (if (string? group) (cl-test group) (cl-test))]
   [("-d" "--persist-dscan" "--save-dscan") "Save raw dscans to disk, default: false"
    (cl-persist #t)]
   [("-P" "--port") port "Use specified port, default: 8000"
    (if (number? (string->number port)) (cl-port (string->number port)) (cl-port))]
   [("-w" "--webroot" "-p" "--prefix") dir "Specify webroot directory, alternative: env EVEROOT=dir, default: current dir"
    (if (directory-exists? dir) (cl-prefix dir) (raise-user-error "[error] Directory does not exist:" dir))]
   #:once-any
   [("-a" "--auth" "--basic-auth") "Use basic auth, default: false"
    (cl-auth #t)]
   [("-l" "--login" "--login-auth") "Use internal auth via login page, default: false"
    (cl-login #t)]))

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
