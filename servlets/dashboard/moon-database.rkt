#lang racket

(require eve)

(require "common.rkt")

(provide (except-out (all-defined-out)
		     sql-columns))

(define sql-columns
  (string-append "regionName,constellationName,solarsystemName,planet,moon,"
		 "allianceTicker,allianceName,corporationTicker,corporationName,"
		 "datetime,typeName,moonType,online,checkStatus,scanID"))

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
	  #:colorise-whitelist #t
	  (list (literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
		(literal (style/inline 'type: "text/css" "td { white-space: normal; }"))
		(literal (style/inline 'type: "text/css" "select { margin-right: 0.5em; }"))
		(literal (style/inline 'type: "text/css" "span { margin: 0 .25em; }"))
		(literal (style/inline 'type: "text/css" "tr.offline, span.offline { color: gray; }"))
		(literal (style/inline 'type: "text/css" "tr.rescan, span.rescan { background-color: orange; }"))
		(literal (style/inline 'type: "text/css" "tr.empty, span.empty { background-color: gray; }"))
		(literal (style/inline 'type: "text/css" "tr.empty.rescan { background-color: gray; color: orange; }"))))
	 (body
	  (output:create-html-navigation
	   #:active (url->string (request-uri req))
	   #:audience (auth:try-authorization-header :subject req)
	   #:links '(("Citadel Database" . "citadel-database")
		     ("Goo Database" . "goo-database")
		     ("Moon Database" . "moon-database")))
	  (output:create-region-filter (sql-get-scanned-regions "moonScanView"))
	  (div 'id: "content"
	       (h1 "Moon Scan Data")
	       (output:create-html-hint (output:create-html-legend :moon))
	       (output:create-html-hint (print-filters :list '("Region" "Constellation" "System" "Alliance" "Corporation" "Type" "Goo")
						       (list
							filter-region
							filter-constellation
							filter-system
							filter-alliance
							filter-corporation
							filter-type
							filter-goo)))
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table
		#:ticker->class #t
		#:drop-right 3
		#:head (list "Region" "Constellation" "System" "Planet" "Moon" "AT"
			     "Alliance" "CT" "Corporation" "Date" "Tower" "Goo")
		(output:entry-add-scanid
		 #:position 10
		 (cond [(and (not (null? user-filter)) (member "intersect" f-mode))
			(sql-get-by-filter user-filter #:table "moonScanView" #:union? #f #:columns sql-columns)]
		       [(not (null? user-filter))
			(sql-get-by-filter user-filter #:table "moonScanView" #:columns sql-columns)]
		       [else (map vector->list (sql-build-query sql-columns : "moonScanView"))])))
	       (output:create-html-hint :updated))))
	port))))

  ;; Parse user input (URL parameters)
  (sql-bind-user-input "region" #:request req)
  (sql-bind-user-input "constellation" #:request req)
  (sql-bind-user-input "system" #:request req)
  (sql-bind-user-input "alliance" #:request req)
  (sql-bind-user-input "corporation" #:request req)
  (sql-bind-user-input "type" #:request req)
  (sql-bind-user-input "goo" #:request req)

  (define f-mode (get-filter req #"mode"))

  (define user-filter
    (append filter-region
	    filter-constellation
	    filter-system
	    filter-alliance
	    filter-corporation
	    filter-type
	    filter-goo))

  (send/back response-generator))
