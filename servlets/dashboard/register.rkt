#lang racket

(require eve)

(require "common.rkt")

(provide (all-defined-out))

;; User registration (main)

(define (exec-register req)
  (define response-generator
    (response/output
     (lambda (out)
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
			  (div 'class: "form-field" (input 'type: "text" 'name: "user" 'required: #t)))
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

  (send/back response-generator))

;; User registration (POST data)

(define (exec-register-post req)
  (define response-generator
    (response/output
     (lambda (out)
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
    (values
     (extract-post-data req #"user")
     (extract-post-data req #"email")
     (extract-post-data req #"pass")
     (extract-post-data req #"pass-confirm")))

  (define passwords-match?
    (if (equal? pass pass-confirm)
	#t
	#f))

  (define user-exists?
    (cond [(false? (auth:sql-auth-get-user user)) #f]
	  [else #t]))

  (when (and passwords-match? (false? user-exists?))
    (begin
      (auth:sql-auth-insert-user (struct-copy scrypt-hash
					      (auth:scrypt-input->hash (string->bytes/utf-8 pass) #:length 32)
					      [user (string-downcase user)]))
      (auth:sql-auth-insert-mail (struct-copy scrypt-hash
					      (auth:scrypt-input->hash (string->bytes/utf-8 email) #:length 32)
					      [user (string-downcase user)]))))

  (send/back response-generator))
