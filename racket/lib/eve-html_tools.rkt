#lang racket

(require srfi/19)
(require db/util/datetime)
(require (only-in db
		  sql-date?))

(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

(provide (prefix-out output: (all-defined-out)))

(define (create-html-table input-list #:head [head null] #:id [id "main"])
  (div 'class: "data"
       (table 'id: id 'class: "tablesorter"
	      (if (null? head) null (thead (tr (map th head))))
	      (tbody (map (lambda (row)
			    (tr
			     (map (lambda (str)
				    (td (cond
					 [(sql-date? str) (date->string (sql-datetime->srfi-date str) "~1")]
					 [(number? str) str]
					 [(regexp-match #px"^http" str) (a 'href: str 'target: "_blank" "-> link")]
					 [else str])))
				  row)))
			  input-list)))))

(define (create-html-head #:title [str "Title"] #:sort-column [sort-column 6])
  (head
   (title str)
   (literal (style/inline 'type: "text/css" ".data { margin: 1em 0; }"))
   (literal (style/inline 'type: "text/css" "table { border-collapse: collapse;  border: 1px solid black; width: 100%; }"))
   (literal (style/inline 'type: "text/css" "thead { border-bottom: 1px solid black; }"))
   (literal (style/inline 'type: "text/css" "td { padding: 0.3em; border-right: 1px solid black; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 15.5em; }"))
   (literal (style/inline 'type: "text/css" "tr:nth-child(2n+1) > td { background-color: #efefef; }"))
   (literal (style/inline 'type: "text/css" "th.header { background: url(\"data:image/gif;base64, R0lGODlhFQAJAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAkAAAIXjI+AywnaYnhUMoqt3gZXPmVg94yJVQAAOw==\") no-repeat 99% ; margin-left: -1px; background-position: center left; padding: .2em 1.33em; text-align: left; } th.headerSortUp { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjB+gC+jP2ptn0WskLQA7\") no-repeat 99% ; } th.headerSortDown { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjI8Bya2wnINUMopZAQA7\") no-repeat 99% ; }"))
   (script 'type: "text/javascript" 'src: "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")
   (script 'type: "text/javascript" 'src: "./jquery.tablesorter.min.js")
   (script (literal (string-append "$(document).ready(function() { $(\"*\").tablesorter( { sortList: [["
				   (number->string sort-column)
				   ",0]] } ); });")))))

(define-syntax create-html-hint
  (syntax-rules (:tablesorter :updated)
    ((_ :tablesorter) (create-html-hint "Hint: hold down SHIFT to select multiple columns for sorting"))
    ((_ :updated) (create-html-hint (string-append "Last updated: " (date->string (current-date) "~4"))))
    ((_ str) (p 'style: "padding-left:.2em" str))))
