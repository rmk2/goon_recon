#lang racket

(require eve)

(require "common.rkt")

(provide (all-defined-out))

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
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table #:id "timers"
					 #:head (list "Alliance" "Structure" "System"
						      "Constellation" "Region" "Date")
					 (user-filter-regions filter_region
							      #:filter-function timerboard-query-region
							      #:function (timerboard-query)))
	       (output:create-html-hint :updated))))
	port))))

  (define filter_region (get-filter req #"region"))

  (send/back response-generator))
