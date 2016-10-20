#lang racket

(require eve)

(require "common.rkt")

(provide (except-out (all-defined-out)
		     sql-columns))

(define sql-columns "regionName,constellationName,solarSystemName,planet,moon,datetime,moonType")

(define (exec-goo-database req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Moon Probing Data"
	  #:tablesorter #t
	  #:navigation #t
	  #:sort-column 0
	  (list (literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
		(literal (style/inline 'type: "text/css" "td { white-space: normal; }"))
		(literal (style/inline 'type: "text/css" "select { margin-right: 0.5em; }"))
		(literal (style/inline 'type: "text/css" "span { margin: 0 .25em; }"))))
	 (body
	  (output:create-html-navigation
	   #:active "goo-database"
	   #:audience (auth:try-authorization-header :subject req)
	   #:links '(("Citadel Database" . "citadel-database")
		     ("Goo Database" . "goo-database")
		     ("Moon Database" . "moon-database")))
	  (output:create-region-filter (sql-get-scanned-regions "moonGooDV"))
	  (div 'id: "content"
	       (h1 "Moon Probing Data")
	       (output:create-html-hint "Note: Entries without a valid datetime field are imported from external data")
	       (output:create-html-hint (print-filters filter-region
						       filter-constellation
						       filter-system))
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table
		#:ticker->class #t
		#:drop-right 0
		#:head (list "Region" "Constellation" "System" "Planet" "Moon" "Date" "Goo")
		(cond [(and (not (null? user-filter)) (member "intersect" f-mode))
		       (sql-get-by-filter user-filter #:table "moonGooDV" #:union? #f #:columns sql-columns)]
		      [(not (null? user-filter))
		       (sql-get-by-filter user-filter #:table "moonGooDV" #:columns sql-columns)]
		      [else (map vector->list (sql-build-query sql-columns : "moonGooDV"))]))
	       (output:create-html-hint :updated))))
	port))))

  ;; Parse user input (URL parameters)
  (sql-bind-user-input "region" #:request req)
  (sql-bind-user-input "constellation" #:request req)
  (sql-bind-user-input "system" #:request req)

  (define f-mode (get-filter req #"mode"))

  (define user-filter
    (append filter-region
	    filter-constellation
	    filter-system))

  (send/back response-generator))
