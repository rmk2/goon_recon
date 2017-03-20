#lang racket

(require srfi/19)
(require db/util/datetime)
(require db)

(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml
		  make-element))

(require "eve-whitelist_tools.rkt")

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
				    (if (and ticker->class?
					     (string? str)
					     (regexp-match? #px"^[A-Z0-9. -_]{1,5}$" str)
					     (not (regexp-match? #px"IHUB|TCU" str)))
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
			  #:forms [forms? #f]
			  #:password-check [password-check? #f]
			  #:colorise-whitelist [colorise-whitelist? #f]
			  [extra-fields null])
  (head
   (meta 'charset: "utf-8")
   (title title-str)
   (style/inline 'type: "text/css" ".data { margin: 1em 0; overflow-x: auto; display: block;}")
   (style/inline 'type: "text/css" "table { border-collapse: collapse;  border: 1px solid black; width: 100%; }")
   (style/inline 'type: "text/css" "thead { border-bottom: 1px solid black; }")
   (style/inline 'type: "text/css" "td { padding: 0.3em; border-right: 1px solid black; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 14em; }")
   (style/inline 'type: "text/css" "tr:nth-child(2n) { background-color: #efefef; }")
   (if tablesorter? (create-html-head-tablesorter sort-column) null)
   (if navigation? (create-html-head-navigation) null)
   (if forms? (create-html-head-form) null)
   (if password-check? (create-html-head-check-password) null)
   (if colorise-whitelist? (create-html-head-colorise-whitelist) null)
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
   (style/inline 'type: "text/css" "#nav { display: flex; flex-flow: column nowrap; border-bottom: 1px solid indianred; }")
   (style/inline 'type: "text/css" "#content { margin: 0.5em; clear: left; }")
   (style/inline 'type: "text/css" ".active { background: lightgray; }")
   (style/inline 'type: "text/css" ".nav-title { font-weight: bold; padding: 0.75em; color: indianred; }")
   (style/inline 'type: "text/css" ".nav-element { padding: 0.75em; }")
   (style/inline 'type: "text/css" ".nav-element:hover { background-color: indianred; }")
   (style/inline 'type: "text/css" ".nav-element a { padding: 0.75em; text-decoration: none; color: black; }")
   (style/inline 'type: "text/css" ".nav-selector:hover { background-color: darkgrey; }")
   (style/inline 'type: "text/css" ".nav-selector::after { content: '↓'; vertical-align: sub; font-size: x-small; }")
   (style/inline 'type: "text/css" ".nav-selector { display: none; padding: 0.7em 1.5em; }")
   (style/inline 'type: "text/css" ".nav-dropdown { display: inline-block; position: static; }")
   (style/inline 'type: "text/css" ".nav-submenu { display: flex; flex-flow: column wrap; background-color: white; }")
   (style/inline 'type: "text/css" ".right .nav-submenu { right: 0; }")
   (style/inline 'type: "text/css" (string-append "@media (min-width: 960px) { "
						  "#nav { flex-flow: row nowrap; } "
						  ".right { margin-left: auto; } "
						  ".nav-selector { display: inline-block; } "
						  ".nav-submenu { display: none; border: 1px solid indianred; } "
						  ".nav-dropdown:hover .nav-submenu { display: flex; position: absolute; } "
						  "}"))))

(define (create-html-head-form #:content-flex? [content-flex? #t]
			       #:form-background? [form-background? #t]
			       #:password-stretch? [password-stretch? #t])
  (list
   (style/inline 'type: "text/css" "#links { display:flex; flex-flow: column nowrap; margin-top: 1em; }")
   (style/inline 'type: "text/css" ".form-description:after { content: ':'; }")
   (style/inline 'type: "text/css" ".form-entry { display: flex; flex-flow: column wrap; margin-bottom: 1em; }")
   (style/inline 'type: "text/css" ".form-password { margin-bottom: 1em; padding: 1em 1em 0; border: 1px solid lightgrey; }")
   (style/inline 'type: "text/css" ".subtitle { margin-bottom: 1em; font-weight: bold; font-size: large; }")
   (style/inline 'type: "text/css" ".info { border: 1px solid black; background-color: whitesmoke; padding: 1.5em; }")
   (literal (style/inline 'type: "text/css" ".info > * { display: flex; justify-content: center; }"))
   (if form-background?
       (style/inline 'type: "text/css" "form { border: 1px solid black; background-color: whitesmoke; padding: 1em; }")
       null)
   (if password-stretch?
       (literal (style/inline 'type: "text/css" " input[type='password'], select { width: 100%; }"))
       null)
   (if content-flex?
       (list
	(style/inline 'type: "text/css" "#content { display: flex; flex-flow: column nowrap; align-items: center;  margin: 0 2em; }")
	(literal (style/inline 'type: "text/css" "#content > p:first-of-type { margin-top: -0.25em; }")))
       null)))

(define (create-html-head-check-password)
  (list
   (style/inline 'type: "text/css" ".form-error { margin-bottom: 1em; color: indianred; display: none; }")
   (script 'type: "text/javascript" (literal "function checkPW(form,e1,e2,error) { var err = document.getElementById(error); if (form.elements[e1].value == form.elements[e2].value ) { err.style.display = 'none'; form.elements['submit'].disabled = false } else { err.style.display = 'block'; form.elements['submit'].disabled = true } };"))))

(define (create-html-head-colorise-whitelist)
  (map (lambda (ticker) (literal (style/inline 'type: "text/css"
					       (format "tr > td[class=\"~a\"] { background-color: #4D6EFF; color: white; }"
						       ticker))))
       (map (lambda (entry) (vector-ref entry 1))
	    (append (sql-auth-get-whitelist-alliances)
		    (sql-auth-get-whitelist-corporations)))))

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

(define (create-html-navigation #:title [nav-title "The Reconing"]
				#:links [nav-list null]
				#:active [active-url null]
				#:audience [nav-audience null]
				#:login? [login? #t])
  (let ([nav-default '(("Dashboard" . "/dscan")
		       ("Report" . "/report")
		       ("Tasks" . "/tasks")
		       ("Timerboard" . "/timers")
		       ("Citadel Database" . "/citadel-database")
		       ("Goo Database" . "/goo-database")
		       ("Moon Database" . "/moon-database"))])
    (div 'id: "nav"
	 (div 'class: "nav-title" nav-title)
	 (map (lambda (x) (make-element
			   'div
			   (if (equal? active-url (regexp-replace #px"/$" (cdr x) ""))
			       (list (cons 'class "active nav-element"))
			       (list (cons 'class "nav-element")))
			   (a 'href: (cdr x) (car x))))
	      (cond
	       [(and (or (false? nav-audience) (null? nav-audience)) (not (null? nav-list))) nav-list]
	       [(and (string? active-url) (regexp-match? #px"^/management/.*" active-url) (not (null? nav-list))) nav-list]
	       [(and (string? active-url) (regexp-match? #px"^/supers/.*" active-url) (not (null? nav-list))) nav-list]
	       [else
		(match nav-audience
		  [(? number? nav-audience)
		   (cond [(>= nav-audience 64) nav-default] ;; recon-l+
			 [(>= nav-audience 32) (drop-right nav-default 3)] ;; recon+
			 [(>= nav-audience 4) (take nav-default 2)] ;; alliance+
			 [else (take nav-default 1)])]
		  [(pregexp #px"recon-l|admin|owner") nav-default]
		  ["recon" (drop-right nav-default 3)]
		  [(pregexp #px"alliance|corporation") (take nav-default 2)]
		  [else (take nav-default 1)])]))
	 (cond [(and (not (false? login?)) (or (number? nav-audience) (string? nav-audience)))
		(div 'class: "nav-element right" (a 'href: "/logout" "Logout"))]
	       [(not (false? login?))
		(div 'class: "nav-element right" (a 'href: "/login" "Login"))]
	       [else null]))))

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

(define (entry-add-killid lst #:position [position 7] #:url [url "https://zkillboard.com/kill/"])
  (map (lambda (scan)
	 (list-update scan position (lambda (type)
				      (let* ([id (list-ref scan position)]
					     [killid (if (number? id) (number->string id) "0")])
					(a 'href: (string-append url killid "/")
					   'target: "_blank"
					   'rel: "noopener noreferrer"
					   killid)))))
       lst))
