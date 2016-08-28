#lang racket

(require eve)

(require "common.rkt")

(provide (all-defined-out))

(define (exec-citadel-database-delete req)
  (let* ([bind-raw (request-bindings/raw req)]
  	 [bind-filter (remove (bindings-assq #"region" bind-raw) bind-raw)]
	 [referer (headers-assq* #"referer" (request-headers/raw req))])
    (begin
      (sql-citadel-delete-scan
       (map (lambda (x) (bytes->string/utf-8 (binding:form-value x))) bind-filter))
      (cond [(and (not (false? referer))
		  (regexp-match? "(?:http|https)://recon.tendollarbond.com/.*"
				 (bytes->string/utf-8 (header-value referer))))
	     (redirect-to (string-replace (url->string (request-uri req)) "/recon" ""))]
	    [else (redirect-to (url->string (request-uri req)))]))))

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
	  (list (literal (style/inline 'type: "text/css" "tr > td[class=\"LOLTX\"], tr > td[class=\"OHGOD\"] { background-color: #4D6EFF; color: white; }"))
		(literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
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
	   #:title "GoonSwarm Recon L"
	   #:active "citadel-database"
	   #:links '(("Citadel Database" . "citadel-database")
		     ("Goo Database" . "goo-database")
		     ("Moon Database" . "moon-database")))
	  (output:create-region-filter (sql-get-scanned-regions "citadelScanView"))
	  (div 'id: "content"
	       (h1 "Citadel Scan Data")
	       (output:create-html-hint (output:create-html-legend :citadel))
	       (output:create-html-hint (format "Results filtered for: Region (~a)"
						(string-join (query-regions filter_region)  "|")))
	       (output:create-html-hint :tablesorter)
	       (form 'method: "POST" 'target: "_self" 'id: "main" 'name: "main"
		     (output:create-html-table #:ticker->class #t
					       #:drop-right 2
					       #:head (list "Region" "Constellation" "System" "Location" "AT"
							    "Alliance" "CT" "Corporation" "Date" "Type" "")
					       (output:entry-add-scanid
						#:position 9
						(user-filter-regions filter_region
								     #:filter-function sql-citadel-region-citadels
								     #:function (map vector->list (sql-citadel-get-citadels)))))
		     (input 'type: "submit" 'id: "submit" 'value: "Delete marked entries"))
	       (output:create-html-hint :updated))))
	port))))

  (define filter_region (get-filter req #"region"))

  (send/back response-generator))
