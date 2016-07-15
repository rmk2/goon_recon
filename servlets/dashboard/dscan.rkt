#lang racket

(require eve)

(require net/uri-codec)

(require "common.rkt")

(provide (all-defined-out))

(define (exec-dscan-report req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head #:title "Dashboard" #:tablesorter #f #:navigation #t)
	 (body
	  (output:create-html-navigation #:title "GoonSwarm Recon" #:active "intel" #:links '(("Dashboard" . "intel")))
	  (div 'id: "content"
	       (h1 "Dashboard")
	       (form 'action: "submit" 'method: "POST" 'target: "_self" 'id: "main" 'novalidate: #f
		     (fieldset
		      (legend "D-Scan reporting")
		      (br)
		      (textarea 'name: "dscan" 'rows: "20" 'cols: "50" 'required: #t)
		      (br)
		      (br)
		      (input 'type: "checkbox" "Checkbox")
		      (br)
		      (br)
		      (input 'type: "submit" 'value: "Submit"))))))
	port))))
  (send/back response-generator))

(define (exec-dscan #:dscan dscan #:location location)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "D-Scan Result"
	  #:tablesorter #f
	  #:navigation #t
	  (list
	   (literal (style/inline 'type: "text/css" ".dscan { display: flex; flex-wrap: wrap; justify-content: flex-start; align-items: baseline; }"))
	   (literal (style/inline 'type: "text/css" ".dscan.vertical { flex-direction: column; border: 1px solid black; }"))
	   (literal (style/inline 'type: "text/css" ".dscan-column { padding: 1em; }"))
	   (literal (style/inline 'type: "text/css" ".hide { display: none; }"))
	   (literal (style/inline 'type: "text/css" ".dscan-element { display: flex; padding: 0.1em; margin: 0.25em; min-width: 20em; }"))
	   (literal (style/inline 'type: "text/css" "button { margin: 1em; align-self: center; }"))
	   (literal (style/inline 'type: "text/css" ".dscan-type { padding: 0.1em; }"))
	   (literal (style/inline 'type: "text/css" ".dscan-count { margin-right: 0.25em; padding: 0.1em; font-weight: bold; }"))
	   (script (literal "function toggleClass(c) { var x = document.getElementsByClassName(c); for (var i = 0; i < x.length; ++i) { x[i].classList.toggle('hide'); } }"))))
	 (body
	  (output:create-html-navigation #:title "GoonSwarm Recon" #:links '(("Dashboard" . "intel")))
	  (div 'id: "content"
	       (h1 (pretty-print-location location))
	       dscan)))
	port))))

  (send/back response-generator))
