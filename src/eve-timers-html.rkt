#! /usr/bin/env racket
#lang racket

(require eve)

(define-syntax clean-date
  (syntax-rules ()
    ((_ str) (if (sql-timestamp? str)
		 (date->string (sql-datetime->srfi-date str) "~1 ~3")
		 str))))

(define (html-output . param)
  (output-xml (doctype 'html))
  (output-xml
   (html
    (output:create-html-head #:title "Fuzzysov Timer Board" #:sort-column 5)
    (body
     (h1 "Fuzzysov Timer Board")
     (output:create-html-hint :tablesorter)
     (table 'id: "timers" 'class: "tablesorter"
	    (thead (tr (th "Alliance")
		       (th "Structure")
		       (th "System")
		       (th "Constellation")
		       (th "Region")
		       (th "Date")))
	    (tbody
	     (map (lambda (data) (tr (map (lambda (str) (td (clean-date str))) data))) (timerboard-query))))
     (output:create-html-hint :updated)))))

(html-output)
