#lang racket

(require eve)

(require "common.rkt")

(provide (all-defined-out))

(define (exec-login req [param ""])
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Login Page"
	  #:tablesorter #f
	  #:navigation #f
	  (list (style/inline 'type: "text/css" ".form-description:after { content: ':'; }")
		(style/inline 'type: "text/css" ".form-entry { display: flex; flex-flow: column nowrap; margin-bottom: 1em; }")
		(style/inline 'type: "text/css" "#content { display: flex; flex-flow: column nowrap; align-items: center;  margin: 0 2em; }")
		(style/inline 'type: "text/css" "form { border: 1px solid black; background-color: whitesmoke; padding: 2em;  }")))
	 ;; (style/inline 'type: "text/css" "h1 { margin-left: 1rem;  }")))
	 (body
	  (div 'id: "content"
	       (h1 "Login Page")
	       (form 'method: "POST"
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Username")
			  (div 'class: "form-field" (input 'type: "text" 'name: "user" 'required: #t 'autofocus: #t)))
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Password")
			  (div 'class: "form-field" (input 'type: "password" 'name: "pass" 'required: #t)))
		     (input 'type: "hidden" 'name: "referer" 'value: referer 'readonly: #t)
		     (input 'type: "submit" 'value: "Submit"))
	       (div 'style: "display:flex; flex-flow: column nowrap;"
		    (a 'href: "register" "Register new user?")))))
	out))))

  (define referer
    (let ([maybe-referer (headers-assq* #"x-referer" (request-headers/raw req))])
      (if (false? maybe-referer)
  	  ""
  	  (bytes->string/utf-8 (header-value maybe-referer)))))

  (send/back response-generator))

(define (exec-login-post req [param ""])
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml
	(html
	 (body
	  (div 'id: "content"
	       (list
		(p "Invalid username and/or password")
		(p (a 'href: "register" "Register"))))))
	out))))
  
  (define-values (user pass referer)
    (values
     (extract-post-data req #"user")
     (extract-post-data req #"pass")
     (extract-post-data req #"referer")))

  (define user-exists?
    (cond [(not (false? (auth:sql-auth-get-user user))) #t]
  	  [else #f]))

  (define user-valid?
    (cond [(and user-exists?
		(let ([data (auth:sql-auth-get-user user)])
		  (scrypt-hash? data)
		  (auth:scrypt-check-hash data (string->bytes/utf-8 pass) #:length 32)))
	   #t]
	  [else #f]))

  (define user-token-result
    (cond [user-valid?
	   (auth:create-token
	    #:subject (auth:sql-auth-get-user-group :name user)
	    #:username user)]
	  [else #f]))

  (if (not (false? user-token-result))
      (redirect-to (if (string-empty? referer) "/dscan" referer)
  		   #:headers (list (auth:create-authorization-header user-token-result)
  				   (cookie->header (make-cookie "access_token" user-token-result #:max-age 600))))
      (send/back response-generator)))
