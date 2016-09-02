#lang racket

(require eve)

(require "common.rkt")

(provide (except-out (all-defined-out)
		     sql-columns))

(define sql-columns "allianceName,structureType,solarSystemName,constellationName,regionName,datetime")

(define (exec-timers req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head #:title "Fuzzysov Timer Board" #:tablesorter #t #:navigation #t #:sort-column 5
				  (list
				   (literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
				   (literal (style/inline 'type: "text/css" "select { margin-right: 0.5em; }"))))
	 (body
	  (output:create-html-navigation #:title "GoonSwarm Recon" #:active "timers")
	  (output:create-region-filter (sql-get-scanned-regions "customTimerboard"))
	  (div 'id: "content"
	       (h1 "Fuzzysov Timer Board")
	       (output:create-html-hint "Note: Sovereignty data is updated every 10 minutes")
	       (output:create-html-hint (print-filters filter-region
						       filter-constellation
						       filter-system))
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table #:id "timers"
					 #:head (list "Alliance" "Structure" "System"
						      "Constellation" "Region" "Date")
					 (cond [(and (not (null? user-filter)) (member "intersect" f-mode))
						(sql-get-by-filter user-filter #:table "customTimerboard" #:union? #f #:columns sql-columns)]
					       [(not (null? user-filter))
						(sql-get-by-filter user-filter #:table "customTimerboard" #:columns sql-columns)]
					       [else (map vector->list (sql-build-query sql-columns : "customTimerboard"))]))
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
