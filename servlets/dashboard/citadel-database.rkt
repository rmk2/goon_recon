#lang racket

(require eve)

(require "common.rkt")

(provide (except-out (all-defined-out)
		     sql-columns))

(define sql-columns
  (string-append "regionName,constellationName,solarsystemName,locationName,"
		 "allianceTicker,allianceName,corporationTicker,corporationName,"
		 "datetime,typeName,citadelID,checkStatus,scanID"))

(define (exec-citadel-database-delete req)
  (let* ([bind-raw (request-bindings/raw req)]
  	 [bind-filter (let ([raw-filter (append-map (lambda (name) (let ([name-bind (bindings-assq name bind-raw)])
								     (if (false? name-bind) null (remove name-bind bind-raw))))
						    (list #"region" #"constellation" #"system" #"alliance" #"corporation"))])
			(if (empty? raw-filter) null raw-filter))])
    (begin
      (sql-citadel-delete-scan
       (map (lambda (x) (bytes->string/utf-8 (binding:form-value x))) bind-filter))
      (redirect-to (url->string (request-uri req))))))

(define (exec-citadel-database req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Citadel Scan Data"
	  #:tablesorter #t
	  #:navigation #t
	  #:sort-column 0
	  #:colorise-whitelist #t
	  (list (literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
		(literal (style/inline 'type: "text/css" "#submit { float: right; }"))
		(literal (style/inline 'type: "text/css" "td { white-space: normal; }"))
		(literal (style/inline 'type: "text/css" "select { margin-right: 0.5em; }"))
		(literal (style/inline 'type: "text/css" "span { margin: 0 .25em; }"))
		(literal (style/inline 'type: "text/css" "tr.offline, span.offline { color: gray; }"))
		(literal (style/inline 'type: "text/css" "tr.rescan, span.rescan { background-color: orange; }"))
		(literal (style/inline 'type: "text/css" "tr.empty, span.empty { background-color: gray; }"))
		(literal (style/inline 'type: "text/css" "tr.empty.rescan { background-color: gray; color: orange; }"))))
	 (body
	  (output:create-html-navigation
	   #:active "citadel-database"
	   #:audience (auth:try-authorization-header :subject req)
	   #:links '(("Citadel Database" . "citadel-database")
		     ("Goo Database" . "goo-database")
		     ("Moon Database" . "moon-database")))
	  (output:create-region-filter (sql-get-scanned-regions "citadelScanView"))
	  (div 'id: "content"
	       (h1 "Citadel Scan Data")
	       (output:create-html-hint (output:create-html-legend :citadel))
	       (output:create-html-hint (print-filters :list '("Region" "Constellation" "System" "Alliance" "Corporation" "Type")
						       (list
							filter-region
							filter-constellation
							filter-system
							filter-alliance
							filter-corporation
							filter-type)))
	       (output:create-html-hint :tablesorter)
	       (form 'method: "POST" 'target: "_self" 'id: "main" 'name: "main"
		     (output:create-html-table
		      #:ticker->class #t
		      #:drop-right 2
		      #:head (list "Region" "Constellation" "System" "Location" "AT"
				   "Alliance" "CT" "Corporation" "Date" "Type" "")
		      (output:entry-add-scanid
		       #:position 9
		       (cond [(and (not (null? user-filter)) (member "intersect" f-mode))
			      (sql-get-by-filter user-filter #:table "citadelScanView" #:union? #f #:columns sql-columns)]
			     [(not (null? user-filter))
			      (sql-get-by-filter user-filter #:table "citadelScanView" #:columns sql-columns)]
			     [else (map vector->list (sql-build-query sql-columns : "citadelScanView"))])))
		     (input 'type: "submit" 'id: "submit" 'value: "Delete marked entries"))
	       (output:create-html-hint :updated))))
	port))))

  ;; Parse user input (URL parameters)
  (sql-bind-user-input "region" #:request req)
  (sql-bind-user-input "constellation" #:request req)
  (sql-bind-user-input "system" #:request req)
  (sql-bind-user-input "alliance" #:request req)
  (sql-bind-user-input "corporation" #:request req)
  (sql-bind-user-input "type" #:request req)

  (define f-mode (get-filter req #"mode"))

  (define user-filter
    (append filter-region
	    filter-constellation
	    filter-system
	    filter-alliance
	    filter-corporation
	    filter-type))

  (send/back response-generator))
