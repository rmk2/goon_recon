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
			      [(and (member "EMPTY" row) (member "RESCAN" row)) (list (cons 'class "empty rescan"))]
			      [(member "OFFLINE" row) (list (cons 'class "offline"))]
			      [(member "RESCAN" row) (list (cons 'class "rescan"))]
			      [(member "EMPTY" row) (list (cons 'class "empty"))]
			      [else null])
			     (map (lambda (str)
				    (if (and (string? str) (regexp-match? #px"^[A-Z0-9. -_]{1,5}$" str) ticker->class?)
					(td 'class: str str)
					(td (cond
					     [(sql-null? str) ""]
					     [(equal? (sql-timestamp 0 0 0 0 0 0 0 #f) str) ""]
					     [(sql-date? str) (date->string (sql-datetime->srfi-date str) "~1")]
					     [(sql-timestamp? str) (date->string (sql-datetime->srfi-date str) "~1 ~3")]
					     [(number? str) str]
					     [(struct? str) str]
					     [(= (bytes-utf-8-length (string->bytes/utf-8 str)) 64)
					      (input 'type: "checkbox" 'form: "main" 'name: str 'value: str "X")]
					     [(regexp-match? #px"^http" str)
					      (a 'href: str 'target: "_blank" 'rel: "noopener noreferrer" "-> link")]
					     [else str]))))
				  (drop-right row drop-amount))))
			  input-list)))))

(define (create-html-head #:title [title-str "Title"]
			  #:sort-column [sort-column 6]
			  #:tablesorter [tablesorter? #t]
			  #:navigation [navigation? #f]
			  [extra-fields null])
  (head
   (meta 'charset: "utf-8")
   (title title-str)
   (literal (style/inline 'type: "text/css" ".data { margin: 1em 0; }"))
   (literal (style/inline 'type: "text/css" "table { border-collapse: collapse;  border: 1px solid black; width: 100%; }"))
   (literal (style/inline 'type: "text/css" "thead { border-bottom: 1px solid black; }"))
   (literal (style/inline 'type: "text/css" "td { padding: 0.3em; border-right: 1px solid black; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 14em; }"))
   (literal (style/inline 'type: "text/css" "tr:nth-child(2n) { background-color: #efefef; }"))
   (if tablesorter? (create-html-head-tablesorter sort-column) null)
   (if navigation? (create-html-head-navigation) null)
   extra-fields))

(define (create-html-head-tablesorter sort-column)
  (list
   (literal (style/inline 'type: "text/css" "th.header { background: url(\"data:image/gif;base64, R0lGODlhFQAJAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAkAAAIXjI+AywnaYnhUMoqt3gZXPmVg94yJVQAAOw==\") no-repeat 99% ; margin-left: -1px; background-position: center left; padding: .2em 1.33em; text-align: left; } th.headerSortUp { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjB+gC+jP2ptn0WskLQA7\") no-repeat 99% ; } th.headerSortDown { background: url(\"data:image/gif;base64, R0lGODlhFQAEAIAAACMtMP///yH5BAEAAAEALAAAAAAVAAQAAAINjI8Bya2wnINUMopZAQA7\") no-repeat 99% ; }"))
   (script 'type: "text/javascript" 'src: "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")
   (script 'type: "text/javascript" 'src: "https://eve.rmk2.org/js/jquery.tablesorter.min.js")
   (script (literal (string-append "$(document).ready(function() { $(\"*\").tablesorter( { sortList: [["
				   (number->string sort-column)
				   ",0]] } ); });")))))

(define (create-html-head-navigation)
  (list
   (style/inline 'type: "text/css" "body { margin: 0; }")
   (style/inline 'type: "text/css" "#nav { width: 100%; display: flex; flex-direction: row; flex-wrap: wrap; border-bottom: 1px solid indianred; }")
   (style/inline 'type: "text/css" "@media (max-width: 680px) { #nav { flex-flow: column nowrap; } }")
   (style/inline 'type: "text/css" "#content { margin: 0.5em; clear: left; }")
   (style/inline 'type: "text/css" ".active { background: lightgray; }")
   (style/inline 'type: "text/css" ".nav-title { font-weight: bold; padding: 0.75em; color: indianred; margin-right: 4em; }")
   (style/inline 'type: "text/css" ".nav-element { padding: 0.75em; }")
   (style/inline 'type: "text/css" ".nav-element:hover { background-color: indianred; }")
   (style/inline 'type: "text/css" ".nav-element a { padding: 0.75em; text-decoration: none; color: black; }")
   (style/inline 'type: "text/css" ".nav-element.right { margin-left: auto; }")))

(define-syntax create-html-hint
  (syntax-rules (:tablesorter :updated)
    ((_ :tablesorter) (create-html-hint "Hint: hold down SHIFT to select multiple columns for sorting"))
    ((_ :updated) (create-html-hint (format "Last updated: ~a" (date->string (current-date) "~4"))))
    ((_ :updated interval) (create-html-hint
			    (format "Last updated: ~a"
				    (date->string (seconds->date (* (quotient (current-seconds) interval) interval)) "~4"))))
    ((_ str) (p 'style: "padding-left:.2em" str))))

(define-syntax create-html-legend
  (syntax-rules (:moon :citadel)
    ((_ :moon) (list "Legend:"
		     (span 'class: "rescan" "Tower needs to be rescanned")
		     (span 'class: "offline" "Tower was offline when scanned")
		     (span 'class: "empty" "Moon was empty when scanned")))
    ((_ :citadel) (list "Legend:"
			(span 'class: "rescan" "Citadel needs to be rescanned")))
    ((_) (create-html-legend :moon))))

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

(define (create-html-navigation #:title [nav-title "GoonSwarm Recon"]
				#:links [nav-list '(("Dashboard" . "dscan")
						    ("Report" . "report")
						    ("Tasks" . "tasks")
						    ("Timerboard" . "timers")
						    ("Citadel Database" . "citadel-database")
						    ("Goo Database" . "goo-database")
						    ("Moon Database" . "moon-database"))]
				#:active [active-url null]
				#:audience [nav-audience null])
  (div 'id: "nav"
       (div 'class: "nav-title" nav-title)
       (map (lambda (x) (make-element
			 'div
			 (if (equal? active-url (cdr x)) (list (cons 'class "active nav-element"))
			     (list (cons 'class "nav-element")))
			 (a 'href: (cdr x) (car x))))
	    (cond [(not (= (length nav-list) 7)) nav-list]
		  [(equal? nav-audience "recon-l") nav-list]
		  [(equal? nav-audience "recon") (drop-right nav-list 3)]
		  [else (take nav-list 1)]))))

(define (create-html-dscan-rows main info structures starbases #:local-scan [local-scan? #f])
  (define (colorise-div #:picker n #:class [class "dscan-element"] body)
    (make-element 'div
		  (list
		   (cons 'class class)
		   (cons 'style
			 (string-append "background-color:"
					(cond
					 [(member n (range 0 10)) "#f5f5f5"] ;; css whitesmoke
					 [(member n (range 10 30)) "#fff7b0"] ;; custom yellow
					 [(member n (range 30 60)) "#ffd700"] ;; css gold
					 [(member n (range 60 100)) "#ffa500"] ;; css orange
					 [else "#ff6347"])))) ;; css tomato
		  body))
  (list
   (div 'class: "dscan horizontal"
	(map (lambda (heading column)
	       (div 'class: "dscan-column"
		    (h3 heading)
		    (map (lambda (element)
			   (colorise-div #:picker (cadr element) #:class "dscan-element"
					 (list
					  (div 'class: "dscan-count" (cdr element))
					  (div 'class: "dscan-type" (car element)))))
			 column)))
	     (if local-scan? (list "Corporations" "Alliances") (list "Ship Types" "Ship Groups"))
	     (if (empty? main) (list null null)
		 (list (car main) (cdr main))))
	(if local-scan?
	    (div 'class: "dscan vertical"
		 (map (lambda (heading column)
			(div 'class: "dscan-column"
			     (h3 heading)
			     (map (lambda (detail)
				    (colorise-div #:picker (cadr detail) #:class "dscan-element"
						  (list
						   (div 'class: "dscan-count" (cdr detail))
						   (div 'class: "dscan-type" (car detail)))))
				  column)))
		      (list "Local Count" "Characters" "Statistics")
		      info))
		      ;; (car info)
		      ;; (cdr info)))
	    (div 'class: "dscan vertical"
		 (map (lambda (heading column)
			(div 'class: "dscan-column"
			     (h3 heading)
			     (map (lambda (group)
				    (div 'class: "dscan-group"
					 (map (lambda (detail)
						(div 'class: "dscan-detail hide"
						     (colorise-div #:picker (cadr detail) #:class "dscan-element"
								   (list
								    (div 'class: "dscan-count" (cdr detail))
								    (div 'class: "dscan-type" (car detail))))))
					      (car group))
					 (map (lambda (element)
						(div 'class: "dscan-summary"
						     (colorise-div #:picker (cadr element) #:class "dscan-element"
								   (list
								    (div 'class: "dscan-count" (cdr element))
								    (div 'class: "dscan-type" (car element))))))
					      (cdr group))))
				  column)))
		      (list "Drones/Deployables" "Structures" "Starbases (on-grid only)")
		      (filter-map (lambda (lst) (if (empty? (flatten lst)) null (filter (lambda (x) (not (empty? x))) lst)))
				  (list info structures starbases)))
		 (button 'onclick: "toggleClass('dscan-detail'); toggleClass('dscan-summary')" "Toggle details"))))))

(define (entry-add-scanid lst #:position [position 10])
  (map (lambda (scan)
	 (list-update scan position (lambda (type)
				      (cond [(sql-null? (last scan)) type]
					    [(sql-null? type)
					     (a 'href: (string-append "/dscan/" (last scan))
						'target: "_blank"
						'rel: "noopener noreferrer"
						"None")]
					    [else
					     (a 'href: (string-append "/dscan/" (last scan))
						'target: "_blank"
						'rel: "noopener noreferrer"
						type)]))))
       lst))
