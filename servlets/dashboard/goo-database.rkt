#lang racket

(require eve)

(require "common.rkt")

(provide (all-defined-out))

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
	   #:title "GoonSwarm Recon L"
	   #:active "goo-database"
	   #:links '(("Citadel Database" . "citadel-database")
		     ("Goo Database" . "goo-database")
		     ("Moon Database" . "moon-database")))
	  (output:create-region-filter (sql-get-scanned-regions "moonGooDV"))
	  (div 'id: "content"
	       (h1 "Moon Probing Data")
	       (output:create-html-hint "Note: Entries without a valid datetime field are imported from external data")
	       (output:create-html-hint (format "Results filtered for: Region (~a)"
						(string-join (query-regions filter_region)  "|")))
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table #:ticker->class #t
					 #:drop-right 0
					 #:head (list "Region" "Constellation" "System" "Planet" "Moon" "Date" "Goo")
					 (user-filter-regions filter_region
							      #:filter-function sql-goo-region-scans
							      #:function (map vector->list (sql-goo-get-scans))))
	       (output:create-html-hint :updated))))
	port))))

  (define filter_region (get-filter req #"region"))

  (send/back response-generator))
