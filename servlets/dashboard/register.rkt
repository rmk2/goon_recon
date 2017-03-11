#lang racket

(require eve)

(require "common.rkt")
(require "sso-auth.rkt")

(provide (all-defined-out))

;; Handle redirects for SSO identification

(define (exec-register-pre req)
  (cond [(not (false? (try-auth-cookie req #:type "access_token")))
	 (redirect-to "login")]
	[(not (false? (try-auth-cookie req #:type "registration_token")))
	 (exec-register req)]
	[(not (null? (request-bindings req)))
	 (exec-auth-token-response req)]
	[else (exec-auth-token-request req #:type "register")]))

;; User registration (main)

(define (exec-register req)
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "User Registration"
	  #:tablesorter #f
	  #:navigation #f
	  #:forms #t)
	 (body
	  (div 'id: "content"
	       (h1 "User Registration")
	       (form 'method: "POST"
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Username")
			  (div 'class: "form-field"
			       (cond [(not (false? maybe-auth-struct))
				      (input 'type: "text" 'value: (recon-jwt-username maybe-auth-struct) 'disabled: #t)]
				     [else (input 'type: "text" 'name: "user" 'required: #t)])))
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Email")
			  (div 'class: "form-field" (input 'type: "text" 'name: "email" 'required: #t)))
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Password")
			  (div 'class: "form-field" (input 'type: "password" 'name: "pass" 'required: #t)))
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Confirm password")
			  (div 'class: "form-field" (input 'type: "password" 'name: "pass-confirm" 'required: #t)))
		     (input 'type: "submit" 'value: "Register"))
	       (div 'id: "links"
		    (a 'href: "login" "Continue to login")))))
	out))))

  (define maybe-auth-struct (try-auth-cookie req #:type "registration_token"))

  (send/back response-generator))

;; User registration (POST data)

(define (exec-register-post req)
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "User Registration"
	  #:tablesorter #f
	  #:navigation #f
	  #:forms #t)
	 (body
	  (div 'id: "content"
	       (h1 "User Registration")
	       (div 'class: "info"
		    (cond [(false? passwords-match?)
			   (list
			    (p 'style: "color:crimson;" (format "[Error] Passwords do not match!"))
			    (p (a 'href: "javascript:window.history.back();" "Return?")))]
			  [user-exists?
			   (p 'style: "color:crimson;" (format "[Error] User '~a' already exists!" user))]
			  [else
			   (p (format "User '~a' (~a) created!" (string-downcase user) email))]))
	       (div 'id: "links"
		    (a 'href: "login" "Continue to login")))))
	out))))

  (define-values (user email pass pass-confirm)
    (let ([maybe-auth-struct (try-auth-cookie req #:type "registration_token")])
      (values
       (if (false? maybe-auth-struct) (extract-post-data req #"user") (recon-jwt-username maybe-auth-struct))
       (extract-post-data req #"email")
       (extract-post-data req #"pass")
       (extract-post-data req #"pass-confirm"))))

  (define passwords-match?
    (if (equal? pass pass-confirm)
	#t
	#f))

  (define user-exists?
    (cond [(false? (auth:sql-auth-get-user user)) #f]
	  [else #t]))

  (when (and passwords-match? (false? user-exists?))
    (let ([hash-data (auth:scrypt-input->hash (string->bytes/utf-8 pass) #:length 32)])
      (auth:sql-auth-insert-user (scrypt-full (string-downcase user)
					      (string-downcase email)
					      (scrypt-hash-input hash-data)
					      (scrypt-hash-salt hash-data)
					      (srfi-date->sql-timestamp (current-date))))))

  (send/back response-generator))
