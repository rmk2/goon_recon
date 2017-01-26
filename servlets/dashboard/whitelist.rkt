#lang racket

(require eve)
(require eve/eve-whitelist_tools)

(require "common.rkt")

(provide (all-defined-out))

;; Pages

(define (exec-whitelist req)
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Whitelist Management"
	  #:tablesorter #t
	  #:navigation #t
	  #:sort-column 5
	  (append
	   (output:create-html-head-form #:content-flex? #f #:password-stretch? #f)
	   (list
	    (style/inline 'type: "text/css" "h1, p { width: 100%; }")
	    (style/inline 'type: "text/css" "#content { display: flex; flex-flow: row wrap; align-items: flex-start; margin: 0 0.5em; }")
	    (style/inline 'type: "text/css" ".container { margin-right: 2em; }")
	    (literal (style/inline 'type: "text/css" ".container:last-child { margin-right: 0; }")))))
	 (body
	  (output:create-html-navigation #:active "/management/whitelist"
					 #:audience (auth:try-authorization-header :subject req)
					 #:links '(("Dashboard" . "/dscan")
						   ("User Groups" . "/management/groups")
						   ("Whitelist" . "/management/whitelist")))
	  (div 'id: "content"
	       (h1 "Whitelist Management")
	       (div 'class: "container"
		    (h2 "Update whitelist")
		    (output:create-html-hint "Format: +|- Ticker|Name|ID")
		    (form 'method: "POST"
			  (div 'class: "form-entry"
			       (div 'class: "form-description" "Alliances")
			       (div 'class: "form-field"
				    (textarea 'name: "alliances" 'rows: 5 'cols: 40 'placeholder: "+ CONDI")))
			  (div 'class: "form-entry"
			       (div 'class: "form-description" "Corporations")
			       (div 'class: "form-field"
				    (textarea 'name: "corporations" 'rows: 5 'cols: 40 'placeholder: "- KarmaFleet")))
			  (input 'type: "submit" 'id: "submit" 'value: "Submit")))
	       (div 'class: "container"
		    (h2 "Current Whitelist")
		    (output:create-html-hint :tablesorter)
		    (output:create-html-table
		     #:head (list "ID" "Ticker" "Name" "Datetime" "Username" "Type")
		     (append-map (lambda (type lst) (map (lambda (x) (append (vector->list x) (list type))) lst))
				 (list "Alliance"
				       "Corporation")
				 (list (sql-auth-get-whitelist-alliances)
				       (sql-auth-get-whitelist-corporations))))
		    (output:create-html-hint :updated)))))
	out))))

  (send/back response-generator))

(define (exec-whitelist-post req)

  (let ([alliances (extract-post-data req #"alliances")]
	[corporations (extract-post-data req #"corporations")]
	[username (auth:try-authorization-header :username req)])
    (write-whitelist #:type "alliance"
		     (check-whitelist #:type "alliance"
				      #:user username
				      (parse-whitelist alliances)))
    (write-whitelist #:type "corporation"
		     (check-whitelist #:type "corporation"
				      #:user username
				      (parse-whitelist corporations))))

  (redirect-to (url->string (request-uri req))))
