#lang racket

(require srfi/19)
(require db/util/datetime)
(require db)

(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml
		  make-element))

(provide (prefix-out output: (all-defined-out)))

(define (create-html-table input-list
			   #:ticker->class [ticker->class? #f]
			   #:head [head null]
			   #:id [id "main"]
			   #:drop-right [drop-amount 0])
  (div 'class: "data"
       (table 'id: id 'class: "tablesorter"
	      (if (null? head) null (thead (tr (map th head))))
	      (tbody (map (lambda (row)
			    (make-element
			     'tr
			     (cond
			      [(and (member "OFFLINE" row) (member "RESCAN" row)) (list (cons 'class "offline rescan"))]
			      [(member "OFFLINE" row) (list (cons 'class "offline"))]
			      [(member "RESCAN" row) (list (cons 'class "rescan"))]
			      [else null])
			     (map (lambda (str)
				    (if (and (string? str) (regexp-match? #px"^[A-Z0-9. -_]{1,5}$" str) ticker->class?)
					(td 'class: str str)
					(td (cond
					     [(sql-null? str) ""]
					     [(sql-date? str) (date->string (sql-datetime->srfi-date str) "~1")]
					     [(sql-timestamp? str) (date->string (sql-datetime->srfi-date str) "~1 ~3")]
					     [(number? str) str]
					     [(regexp-match #px"^http" str) (a 'href: str 'target: "_blank" "-> link")]
					     [else str]))))
				  (drop-right row drop-amount))))
			  input-list)))))

(define (create-html-head #:title [str "Title"]
			  #:sort-column [sort-column 6]
			  #:tablesorter [tablesorter? #t]
			  [extra-fields null])
  (head
   (meta 'charset: "utf-8")
   (title str)
   (literal (style/inline 'type: "text/css" ".data { margin: 1em 0; }"))
   (literal (style/inline 'type: "text/css" "table { border-collapse: collapse;  border: 1px solid black; width: 100%; }"))
   (literal (style/inline 'type: "text/css" "thead { border-bottom: 1px solid black; }"))
   (literal (style/inline 'type: "text/css" "td { padding: 0.3em; border-right: 1px solid black; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 14em; }"))
   (literal (style/inline 'type: "text/css" "tr:nth-child(2n) { background-color: #efefef; }"))
   (if tablesorter?
       (list
	(literal (style/inline 'type: "text/css" "th.header { background: url(\"data:image/gif;base64, R0lGODlhFQAJAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAkAAAIXjI+AywnaYnhUMoqt3gZXPmVg94yJVQAAOw==\") no-repeat 99% ; margin-left: -1px; background-position: center left; padding: .2em 1.33em; text-align: left; } th.headerSortUp { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjB+gC+jP2ptn0WskLQA7\") no-repeat 99% ; } th.headerSortDown { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjI8Bya2wnINUMopZAQA7\") no-repeat 99% ; }"))
	(script 'type: "text/javascript" 'src: "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")
	(script 'type: "text/javascript" 'src: "./jquery.tablesorter.min.js")
	(script (literal (string-append "$(document).ready(function() { $(\"*\").tablesorter( { sortList: [["
					(number->string sort-column)
					",0]] } ); });"))))
       null)
   extra-fields))

(define-syntax create-html-hint
  (syntax-rules (:tablesorter :updated)
    ((_ :tablesorter) (create-html-hint "Hint: hold down SHIFT to select multiple columns for sorting"))
    ((_ :updated) (create-html-hint (string-append "Last updated: " (date->string (current-date) "~4"))))
    ((_ str) (p 'style: "padding-left:.2em" str))))

(define-syntax create-html-legend
  (syntax-rules ()
    ((_) (list "Legend:"
	       (span 'class: "rescan" "Tower needs to be rescanned")
	       (span 'class: "offline" "Tower was offline when scanned")))))

(define (create-region-filter region-list)
  (div 'id: "bar"
       (form 'name: "filter" 'method: "GET"
	     (label 'for: "region" "Select Region: ")
	     (select 'name: "region"
		     (optgroup 'label: "Show all regions" (option 'value: "" "Any"))
		     (optgroup 'label: "K-Space"
			       (map option (filter (lambda (x) (regexp-match "^(?![A-Z0-9]-)" x)) region-list)))
		     (optgroup 'label: "W-Space"
			       (map option (filter (lambda (x) (regexp-match "^[A-Z0-9]-" x)) region-list))))
	     (input 'type: "submit"))))
