#!/usr/bin/env racket
#lang racket

(require eve/eve-html_tools)
(require net/cgi)
(require scribble/html)

;; Output

(output-http-headers)
(output-xml (doctype 'html))
(output-xml
 (html
  (output:create-html-head #:title "Dashboard")
  (body
   (div 'id: "content"
	(h1 "Dashboard")
	(form 'action: "parse.rkt" 'method: "post" 'target: "_self" 'id: "main" 'novalidate: #t
	      (fieldset
	       (legend "D-Scan reporting")
	       (br)
	       "Corporation Ticker: "
	       (input 'type: "text" 'name: "corporation" 'maxlength: "5" 'size: "5" 'required: #f 'autocomplete: "on" 'style: "margin-right:1em;")
	       "Alliance Ticker: "
	       (input 'type: "text" 'name: "alliance" 'maxlength: "5" 'size: "5" 'required: #f 'autocomplete: "on")
	       (br)
	       (br)
	       (textarea 'name: "dscan" 'rows: "20" 'cols: "50")
	       (br)
	       (br)
	       (input 'type: "checkbox" 'name: "checkbox" 'value: "chechbox" "Checkbox")
	       (br)
	       (br)
	       ;; (input 'type: "checkbox" 'name: "empty" 'value: "empty" "No tower (empty moon)")
	       ;; (br)
	       ;; (br)
	       (input 'type: "submit" 'value: "Submit")))))))
