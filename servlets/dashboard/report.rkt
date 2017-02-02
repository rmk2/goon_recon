#lang racket

(require eve)

(provide (all-defined-out))

(define (exec-report req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Recon Structure Reporting"
	  #:tablesorter #f
	  #:navigation #t
	  #:forms #t
	  (list
	   (style/inline 'type: "text/css" ".form-entry { flex-flow: row wrap; }")
	   (style/inline 'type: "text/css" ".form-field { margin-left: 0.5em; }")
	   (style/inline 'type: "text/css" "#content { align-items: flex-start;  margin: 0 0.5em; }")))
	 (body
	  (output:create-html-navigation #:active "report"
					 #:audience (auth:try-authorization-header :subject req)
					 #:links '(("Report" . "report")
						   ("Tasks" . "tasks")
						   ("Timerboard" . "timers")))
	  (div 'id: "content"
	       (h1 "Recon Structure Reporting")
	       (output:create-html-hint "Hint: Paste citadel dscan, tower dscan or moon probing result")
	       (form 'method: "POST" 'target: "_self" 'id: "main" 'novalidate: #f
		     (div 'class: "subtitle" "D-Scan Reporting")
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Corporation Ticker")
			  (div 'class: "form-field"
			       (input 'type: "text" 'name: "corporation" 'maxlength: "5" 'size: "5" 'required: #f)))
		     (div 'class: "form-entry"
			  (textarea 'name: "dscan" 'rows: "20" 'cols: "50" 'required: #t 'placeholder: "Paste scan data here"))
		     (div 'class: "form-entry"
			  (input 'type: "checkbox" "Checkbox"))
		     (input 'type: "submit" 'value: "Submit")))))
	port))))
  (send/back response-generator))
