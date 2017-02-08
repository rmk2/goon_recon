#lang racket

(require eve)

(require "common.rkt")

(provide (except-out (all-defined-out)
		     sql-columns))

(define sql-columns "regionName,constellationName,solarSystemName,structureType,allianceTicker,allianceName,datetime")

(define update-interval (make-parameter 600))

(define (exec-timers req)
  (define response-generator
    (response/output
     (lambda (port)
       (output-xml (doctype 'html) port)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Fuzzysov Timer Board"
	  #:tablesorter #t
	  #:navigation #t
	  #:sort-column 6
	  #:colorise-whitelist #t
	  (list
	   (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }")
	   (style/inline 'type: "text/css" "select { margin-right: 0.5em; }")))
	 (body
	  (output:create-html-navigation #:active (url->string (request-uri req))
					 #:audience (auth:try-authorization-header :subject req)
					 #:links '(("Report" . "report")
						   ("Tasks" . "tasks")
						   ("Timerboard" . "timers")))
	  (output:create-region-filter (sql-get-scanned-regions "sovTimerboardView"))
	  (div 'id: "content"
	       (h1 "Fuzzysov Timer Board")
	       (output:create-html-hint (format "Note: Sovereignty data is updated every ~a minutes" (/ (update-interval) 60)))
	       (output:create-html-hint (print-filters :list '("Region" "Constellation" "System" "Structure" "Alliance")
						       (list
							filter-region
							filter-constellation
							filter-system
							filter-structure
							filter-alliance)))
	       (output:create-html-hint :tablesorter)
	       (output:create-html-table
		#:id "timers"
		#:ticker->class #t
		#:head (list "Region" "Constellation" "System" "Structure" "A-T" "Alliance" "Date")
		(cond [(and (not (null? (flatten user-filter))) (member "union" f-mode))
		       (sql-get-by-filter user-filter #:table "sovTimerboardView" #:union? #t #:columns sql-columns)]
		      [(not (null? (flatten user-filter)))
		       (sql-get-by-filter user-filter #:table "sovTimerboardView" #:columns sql-columns)]
		      [else (map vector->list (sql-build-query sql-columns : "sovTimerboardView"))]))
	       (output:create-html-hint :updated (update-interval)))))
	port))))

  ;; Parse user input (URL parameters)
  (sql-bind-user-input "region" #:request req)
  (sql-bind-user-input "constellation" #:request req)
  (sql-bind-user-input "system" #:request req)
  (sql-bind-user-input "structure" #:request req)
  (sql-bind-user-input "alliance" #:request req)

  (define f-mode (get-filter req #"mode"))

  (define user-filter
    (list filter-region
	  filter-constellation
	  filter-system
	  filter-structure
	  filter-alliance))

  (send/back response-generator))
