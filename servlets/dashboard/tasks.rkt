#lang racket

(require eve)

(require "common.rkt")

(provide (except-out (all-defined-out)
		     sql-columns))

(define sql-columns
  (string-append "regionName,constellationName,solarSystemName,planet,moon,"
		 "allianceTicker,corporationTicker,datetime,typeName"))

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
	  (output:create-html-navigation #:active (url->string (request-uri req))
					 #:audience (auth:try-authorization-header :subject req)
					 #:links '(("Report" . "report")
						   ("Tasks" . "tasks")
						   ("Timerboard" . "timers")))
	  (output:create-region-filter (sql-get-scanned-regions "moonScanTasks"))
	  (div 'id: "content"
	       (h1 "Recon Moon Scanning Tasks")
	       (output:create-html-hint "Note: All towers below have been killed since their respective last scan. Fields marked with a \"*\" stem from the most recent scan prior to their death.")
	       (output:create-html-hint (print-filters filter-region
						       filter-constellation
						       filter-system))
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table
		#:ticker->class #t
		#:drop-right 0
		#:head (list "Region" "Constellation" "System" "Planet" "Moon"
			     "A-T*" "C-T*" "Date*" "Tower*")
		(cond [(and (not (null? user-filter)) (member "intersect" f-mode))
		       (sql-get-by-filter user-filter #:table "moonScanTasks" #:union? #f #:columns sql-columns)]
		      [(not (null? user-filter))
		       (sql-get-by-filter user-filter #:table "moonScanTasks" #:columns sql-columns)]
		      [else (map vector->list (sql-build-query sql-columns : "moonScanTasks"))]))
	       (output:create-html-hint :updated))))
	port))))

  ;; Parse user input (URL parameters)
  (sql-bind-user-input "region" #:request req)
  (sql-bind-user-input "constellation" #:request req)
  (sql-bind-user-input "system" #:request req)

  (define f-mode (get-filter req #"mode"))

  (define user-filter
    (filter-not null?
		(list filter-region
		      filter-constellation
		      filter-system)))

  (send/back response-generator))
