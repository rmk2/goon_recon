#lang racket

(require eve)
(require "../../lib/eve-auth_basic.rkt")

(require "common.rkt")

(provide (all-defined-out))

;; User registration (main)

(define (exec-register req)
  (response/output
   (lambda (out)
     (output-xml
      (html
       (output:create-html-head
	#:title "User Registration"
	#:tablesorter #f
	#:navigation #f
	(list (style/inline 'type: "text/css" ".form-description:after { content: ':'; }")
	      (style/inline 'type: "text/css" ".form-entry { display: flex; flex-flow: column nowrap; margin-bottom: 1em; }")))
       (body (h1 "User Registration")
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
		   (input 'type: "submit" 'value: "Register"))))
      out))))

;; User registration (POST data)

(define (exec-register-post req)
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml
	(html
	 (output:create-html-head #:title "User Registration"
				  #:tablesorter #f
				  #:navigation #f)
	 (body
	  (h1 "User Registration")
	  (cond [(false? user-exists?)
		 (list
		  (p (format "User '~a' (~a) created!" user email)))]
		[else
		 (list
		  (p 'style: "color:red;" (format "[Error] User '~a' already exists!" user)))])))
	out))))

  (define-values (user email pass)
    (values
     (extract-post-data req #"user")
     (extract-post-data req #"email")
     (extract-post-data req #"pass")))

  (define user-exists?
    (cond [(false? (auth:sql-auth-get-user (bytes->string/utf-8 user))) #f]
	  [else #t]))

  (when (false? user-exists?)
    (begin
      (auth:sql-auth-insert-user (struct-copy scrypt-hash
					      (auth:scrypt-input->hash pass #:length 32)
					      [user (bytes->string/utf-8 user)]))
      (auth:sql-auth-insert-mail (struct-copy scrypt-hash
					      (auth:scrypt-input->hash email #:length 32)
					      [user (bytes->string/utf-8 user)]))))

  (send/back response-generator))
