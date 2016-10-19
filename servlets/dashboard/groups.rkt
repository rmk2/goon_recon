#lang racket

(require racket/set)

(require eve)
(require "../../lib/eve-auth_basic.rkt")

(require (prefix-in html: (only-in scribble/html/xml
				   make-element)))

(require "common.rkt")

(provide (all-defined-out))

(define (exec-groups-modify req)
  (let ([orig-data (map vector->list (auth:sql-auth-get-groups))]
	[update-data (map (lambda (x)
			    (list (bytes->string/utf-8 (binding-id x))
				  (bytes->string/utf-8 (binding:form-value x))))
			  (request-bindings/raw req))])
    (begin
      (auth:sql-auth-update-groups (set-subtract update-data orig-data))
      (redirect-to (url->string (request-uri req))))))

(define (exec-groups req)
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Auth Group Management"
	  #:tablesorter #t
	  #:navigation #f
	  #:sort-column 0
	  (list (style/inline 'type: "text/css" ".group-name { font-weight: bold; margin: 0 0 .1em; max-width: 9.5em; overflow: hidden; }")
		(style/inline 'type: "text/css" ".group-list { display: flex; flex-flow: row wrap; margin-bottom: 0.5em; }")
		(style/inline 'type: "text/css" ".group-entry { display: flex; flex-flow: column nowrap; margin: 0 1em .5em 0; padding: .25em; background-color: whitesmoke; border: 1px solid lightgrey; }")))
	 (body
	  (div 'id: "content"
	       (h1 "Auth Group Management")
	       (form 'method: "POST" 'target: "_self" 'id: "main" 'name: "main"
		     (div 'class: "group-list"
		     	  (map (lambda (x)
				 (let* ([user (vector-ref x 0)]
					[audience (vector-ref x 1)])
				   (div 'class: "group-entry"
					(div 'class: "group-name" (string-titlecase user))
					(div 'class: "group-select"
					     (select 'name: user
						     (optgroup 'label: "Unaffiliated"
							       (map (lambda (x)
								      (make-group-options (car x) (cdr x) audience))
								    '(("public" . "Public"))))
						     (optgroup 'label: "Affiliated"
							       (map (lambda (x)
								      (make-group-options (car x) (cdr x) audience))
								    '(("corporation" . "Corporation")
								      ("alliance" . "Alliance"))))
						     (optgroup 'label: "Recon"
							       (map (lambda (x)
								      (make-group-options (car x) (cdr x) audience))
								    '(("recon-l" . "Recon Leadership")
								      ("recon" . "Recon Member"))))
						     (optgroup 'label: "Command"
							       (map (lambda (x)
								      (make-group-options (car x) (cdr x) audience))
								    '(("admin" . "Administrator")))))))))
		     	       (auth:sql-auth-get-groups)))
		     (hr)
		     (input 'type: "submit" 'id: "submit" 'value: "Save group memberships")))))
	out))))

  (define (make-group-options value str audience)
    (html:make-element
     'option (list (cons 'value value)
		   (if (equal? value audience)
		       (cons 'selected #t)
		       (cons 'selected #f)))
     str))

  (send/back response-generator))
