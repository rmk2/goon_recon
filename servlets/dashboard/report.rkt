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
	 (output:create-html-head #:title "Recon Structure Reporting" #:tablesorter #f #:navigation #t)
	 (body
	  (output:create-html-navigation #:active "report"
					 #:audience (auth:try-authorization-header :subject req)
					 #:links '(("Report" . "report")
						   ("Tasks" . "tasks")
						   ("Timerboard" . "timers")))
	  (div 'id: "content"
	       (h1 "Recon Structure Reporting")
	       (form 'method: "POST" 'target: "_self" 'id: "main" 'novalidate: #f
		     (fieldset
		      (legend "D-Scan reporting")
		      (br)
		      "Corporation Ticker: "
		      (input 'type: "text" 'name: "corporation" 'maxlength: "5" 'size: "5" 'required: #f 'autocomplete: "on" 'style: "margin-right:1em;")
		      "Alliance Ticker: "
		      (input 'type: "text" 'name: "alliance" 'maxlength: "5" 'size: "5" 'required: #f 'autocomplete: "on")
		      (br)
		      (br)
		      (textarea 'name: "dscan" 'rows: "20" 'cols: "50" 'required: #t)
		      (br)
		      (br)
		      (input 'type: "checkbox" "Checkbox")
		      (br)
		      ;; (input 'type: "checkbox" 'name: "empty" 'value: "empty" "Empty moon (no tower)")
		      ;; (br)
		      ;; (p "Note: only enter a location if no celestial appears on D-Scan")
		      ;; "Location: "
		      ;; (input 'type: "text" 'name: "location" 'required: #f 'autocomplete: "on" 'style: "margin-right:1em;")
		      ;; (br)
		      (br)
		      (input 'type: "submit" 'value: "Submit"))))))
	port))))
  (send/back response-generator))
