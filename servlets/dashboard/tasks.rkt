#lang racket

(require eve)

(require "common.rkt")

(provide (all-defined-out))

(define (exec-tasks req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Recon Moon Scanning Tasks"
	  #:tablesorter #t
	  #:navigation #t
	  #:sort-column 0
	  (list (literal (style/inline 'type: "text/css" "tr > td[class=\"LOLTX\"], tr > td[class=\"OHGOD\"] { background-color: #4D6EFF; color: white; }"))
		(literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
		(literal (style/inline 'type: "text/css" "td { white-space: normal; }"))
		(literal (style/inline 'type: "text/css" "select { margin-right: 0.5em; }"))))
	 (body
	  (output:create-html-navigation #:title "GoonSwarm Recon" #:active "tasks")
	  (output:create-region-filter (sql-get-scanned-regions "moonScanTasks"))
	  (div 'id: "content"
	       (h1 "Recon Moon Scanning Tasks")
	       (output:create-html-hint "Note: All towers below have been killed since their respective last scan. Fields marked with a \"*\" stem from the most recent scan prior to their death.")
	       (output:create-html-hint (format "Results filtered for: Region (~a)"
						(string-join (query-regions filter_region)  "|")))
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table #:ticker->class #t
					 #:drop-right 0
					 #:head (list "Region" "Constellation" "System" "Planet" "Moon"
						      "A-T*" "C-T*" "Date*" "Tower*")
					 (user-filter-regions filter_region
							      #:filter-function sql-moon-region-tasks
							      #:function (map vector->list (sql-moon-get-tasks))))
	       (output:create-html-hint :updated))))
	port))))

  (define filter_region (get-regions req))

  (send/back response-generator))
