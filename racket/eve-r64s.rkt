#! /usr/bin/env racket
#lang racket

(require eve)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

;; Read from stdin

(define pipe-input (cdr (append* (second (read)))))

;; Ony show us (known) money moons, append their type to the killmail data

(define (query-money-moons [input-list pipe-input])
  (filter-map (lambda (x)
		(let ([moon (vector-ref (parse-moon (list-ref x 7)) 0)])
		  (if (money-moon? moon)
		      (flatten (append x (last (vector->list (parse-moondata (split-moon-display moon))))))
		      #f)))
	      input-list))

;; Filter uniques, keep the newer result

(define (filter-unique lst)
  (reverse
   (remove-duplicates (reverse lst)
		      #:key (lambda (x) (eighth x)))))

;; Parse typeIDs

(define (parse-money-moons input-list)
  (map (lambda (kill) (list
		       (parse-type :name (list-ref kill 0))
		       (list-ref kill 4)
		       (list-ref kill 6)
		       (simplify-moon-display (parse-moon :name (list-ref kill 7)))
		       (parse-region :name (list-ref kill 9))
		       (list-ref kill 10)
		       (last kill)
		       (string-append "https://zkillboard.com/kill/" (number->string (list-ref kill 11)))))
       input-list))

;; HTML Output

(define (create-html-table lst)
  (div 'class: "data"
       (h2 "Losses")
       (table 'id: "losses" 'class: "tablesorter"
	      (thead (tr (th "Type")
			 (th "Corporation")
			 (th "Alliance")
			 (th "System")
			 (th "Region")
			 (th "Date")
			 (th "Goo")
			 (th "Link")))
	      (tbody
	       (map (lambda (data) (tr (map (lambda (str) (td (if (regexp-match #px"^http" str)
								  (a 'href: str 'target: "_blank" "-> link")
								  str))) data)))
		    lst)))))

(define (output-html lst)
  (begin
    (output-xml (doctype 'html))
    (output-xml
     (html
      (head
       (title "EVE Killboard Digest")
       (literal (style/inline 'type: "text/css" ".data { margin: 1em 3%; }"))
       (literal (style/inline 'type: "text/css" "table { border-collapse: collapse;  border: 1px solid black; width: 100%; }"))
       (literal (style/inline 'type: "text/css" "thead { border-bottom: 1px solid black; }"))
       (literal (style/inline 'type: "text/css" "td { padding: 0.3em; border-right: 1px solid black; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 15.5em; }"))
       (literal (style/inline 'type: "text/css" "tr:nth-child(2n+1) > td { background-color: #efefef; }"))
       (literal (style/inline 'type: "text/css" "th.header { background: url(\"data:image/gif;base64, R0lGODlhFQAJAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAkAAAIXjI+AywnaYnhUMoqt3gZXPmVg94yJVQAAOw==\") no-repeat 99% ; margin-left: -1px; background-position: center left; padding: .2em 1.33em; text-align: left; } th.headerSortUp { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjB+gC+jP2ptn0WskLQA7\") no-repeat 99% ; } th.headerSortDown { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjI8Bya2wnINUMopZAQA7\") no-repeat 99% ; }"))
       (script 'type: "text/javascript" 'src: "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")
       (script 'type: "text/javascript" 'src: "./jquery.tablesorter.min.js")
       ;; (script (literal "$(document).ready(function() { $(\"#killmails\").tablesorter( { sortList: [[6,0]] } ); });")))
       (script (literal "$(document).ready(function() { $(\"*\").tablesorter( { sortList: [[6,0]] } ); });")))
      (body
       (div 'id: "content"
	    (h1 "EVE Money Moon Digest")
	    (p 'style: "padding-left:.2em" "Hint: hold down SHIFT to select multiple columns for sorting")
	    (create-html-table lst)))))))

;; Exec

(output-html (parse-money-moons (filter-unique (query-money-moons))))
