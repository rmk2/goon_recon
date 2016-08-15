#lang racket

(require eve)

(require "common.rkt")

(provide (all-defined-out))

(define (exec-moon-database req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Moon Scan Data"
	  #:tablesorter #t
	  #:navigation #t
	  #:sort-column 0
	  (list (literal (style/inline 'type: "text/css" "tr > td[class=\"LOLTX\"], tr > td[class=\"OHGOD\"] { background-color: #4D6EFF; color: white; }"))
		(literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
		(literal (style/inline 'type: "text/css" "td { white-space: normal; }"))
		(literal (style/inline 'type: "text/css" "select { margin-right: 0.5em; }"))
		(literal (style/inline 'type: "text/css" "span { margin: 0 .25em; }"))
		(literal (style/inline 'type: "text/css" "tr.offline, span.offline { color: gray; }"))
		(literal (style/inline 'type: "text/css" "tr.rescan, span.rescan { background-color: orange; }"))
		(literal (style/inline 'type: "text/css" "tr.empty, span.empty { background-color: gray; }"))
		(literal (style/inline 'type: "text/css" "tr.empty.rescan { background-color: gray; color: orange; }"))))
	 (body
	  (output:create-html-navigation
	   #:title "GoonSwarm Recon L"
	   #:active "moon-database"
	   #:links '(("Goo Database" . "goo-database") ("Moon Database" . "moon-database")))
	  (output:create-region-filter (sql-get-scanned-regions "moonScanView"))
	  (div 'id: "content"
	       (h1 "Moon Scan Data")
	       (output:create-html-hint (output:create-html-legend :moon))
	       (output:create-html-hint (format "Results filtered for: Region (~a)"
						(string-join (query-regions filter_region)  "|")))
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table #:ticker->class #t
					 #:drop-right 3
					 #:head (list "Region" "Constellation" "System" "Planet" "Moon" "AT"
						      "Alliance" "CT" "Corporation" "Date" "Tower" "Goo")
					 (output:entry-add-scanid #:position 10
					  (user-filter-regions filter_region
							       #:filter-function sql-moon-region-towers
							       #:function (map vector->list (sql-moon-get-towers)))))
	       (output:create-html-hint :updated))))
	port))))

  (define filter_region (get-regions req))

  (send/back response-generator))
