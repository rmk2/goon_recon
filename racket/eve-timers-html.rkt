#! /usr/bin/env racket
#lang racket

(require net/url)
(require scribble/html)

(define api
  (let ([file "/var/www/servers/eve.rmk2.org/pages/sov_timers.txt"])
    (if (file-exists? file)
	(file->lines file)
	(call/input-url (string->url "https://eve.rmk2.org/sov_timers.txt") get-pure-port port->lines))))


(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

(define-syntax clean-date
  (syntax-rules ()
    ((_ str) (if (regexp-match #px"^[0-9]{4}" str)
		 (string-join (string-split str "T"))
		 str))))

(define (html-output . param)
  (output-xml (doctype 'html))
  (output-xml
   (html
    (head
     (title "Fuzzysov Timer Board")
     (literal (style/inline type: "text/css"
			    "td { border: 1px solid black; padding: .25em; min-width: 5em; } tr:nth-child(2n) > td { background-color:#efefef; } th { border: 1px solid black; } th.header { background: url(\"data:image/gif;base64, R0lGODlhFQAJAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAkAAAIXjI+AywnaYnhUMoqt3gZXPmVg94yJVQAAOw==\") no-repeat 99% ; margin-left: -1px; background-position: center left; padding: .2em 1.33em; text-align: left; } th.headerSortUp { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjB+gC+jP2ptn0WskLQA7\") no-repeat 99% ; } th.headerSortDown { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjI8Bya2wnINUMopZAQA7\") no-repeat 99% ; }"))
     (script 'type: "text/javascript" 'src: "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")
     (script 'type: "text/javascript" 'src: "./jquery.tablesorter.min.js")
     ;; (script 'type: "text/javascript" 'src: "https://raw.githubusercontent.com/christianbach/tablesorter/master/jquery.tablesorter.min.js")
     (script (literal "$(document).ready(function() { $(\"#timers\").tablesorter( { sortList: [[5,0], [1,0]] } ); });")))
    (body
     (h1 "Fuzzysov Timer Board")
     (p 'style: "padding-left:.2em" "Hint: hold down SHIFT to select multiple columns for sorting") 
     (table 'id: "timers" 'class: "tablesorter"
	    (thead (tr (th "Alliance")
		       (th "Structure")
		       (th "System")
		       (th "Constellation")
		       (th "Region")
		       (th "Date")))
	    (tbody
	     (map (lambda (data) (tr (map (lambda (str) (td (clean-date str))) data))) (input-map-split api))))))))

(html-output)

