#!/usr/bin/env racket
;; #lang web-server
#lang racket

(require web-server/servlet
         web-server/servlet-env
	 web-server/private/mime-types)

(require net/uri-codec)

(require eve)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

;; URL dispatch

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("report") exec-report]
   [("result") #:method "post" exec-result]
   [("moon-database") exec-moon-database]
   [("timers") exec-timers]))

(define (main req)
  (main-dispatch req))

;; Parameters

(define max_distance (make-parameter 10000))
(define webroot (if (string? (getenv "EVEROOT")) (getenv "EVEROOT") "./"))

;; Mimetype table

(define system-mime-types (read-mime-types "/etc/mime.types"))

;; d-scan -> scan data

(define (guess-or-location? lst location)
  (let ([guess (guess->location (dscan-guess-location lst))])
    (cond
     [(null? guess) (if (system? location)
			(let ([location-base (parse-solarsystem location)])
			  (list (parse-universe :region location-base)
				(parse-universe :constellation location-base)
				(parse-universe :id location-base)))
			null)]
     [else guess])))

(define (moonscan input #:corporation corporation #:alliance alliance)
  (if (< (hash-ref (dscan-proximity (moon? input)) 'distance) (max_distance))
      (let ([moon (hash-ref (dscan-proximity (moon? input)) 'name)]
	    [tower (hash-ref (dscan-proximity (tower? input)) 'type)])
	(flatten
	 (list
	  (parse-map :region moon)
	  (parse-map :constellation moon)
	  (parse-map :system moon)
	  (cdr (split-moon-display moon))
	  (fill-alliance (list alliance) (list corporation))
	  ;;	  (if (string-empty? alliance) sql-null (string-upcase alliance))
	  (if (string-empty? corporation) sql-null (string-upcase corporation))
	  (srfi-date->sql-timestamp (current-date))
	  (parse-type :id tower)
	  (if (and (dscan-proximity (forcefield? input))
		   (< (hash-ref (dscan-proximity (forcefield? input)) 'distance) (max_distance)))
	      1
	      0))))
      #f))

(define (citadelscan input #:corporation corporation #:alliance alliance #:location location)
  (if (< (hash-ref (dscan-proximity (citadel? input)) 'distance) (max_distance))
      (let ([citadel (hash-ref (dscan-proximity (citadel? input)) 'type)])
	(flatten
	 (list
	  (guess-or-location? input location)
	  "locationid"
	  (fill-alliance (list alliance) (list corporation))
	  (if (string-empty? corporation) sql-null (string-upcase corporation))
	  (srfi-date->sql-timestamp (current-date))
	  (parse-type :id citadel))))
      #f))

;; Check if a given query string is a valid solarSystemName

;; (define (system? query) (findf (lambda (str) (equal? (string-downcase query) (string-downcase str)))
;; 			       (map vector->values
;; 				    (query-rows sqlc "SELECT DISTINCT solarSystemName FROM mapSolarSystems"))))

(define (system? query)
  (if (false? (query-maybe-row sqlc "SELECT solarSystemName FROM mapSolarSystems WHERE solarSystemName = ?" query))
      #f
      query))

;; Pretty-print condensed d-scan result for HTML output

(define (pretty-print-moon-result data result)
  (format "~a: ~a~a @ ~akm, belonging to ~a"
	  (hash-ref (dscan-proximity (moon? data)) 'name)
	  (hash-ref (dscan-proximity (tower? data)) 'type)
	  (if (zero? (tenth result))
	      " (offline) "
	      " (online) ")
	  (hash-ref (dscan-proximity (tower? data)) 'distance)
	  (if (sql-null? (seventh result))
	      "-"
	      (string-append
	       (let ([corporation (parse-corporation (seventh result))])
		 (if (false? corporation)
		     "? "
		     (vector-ref corporation 2)))
	       "["
	       (seventh result)
	       "]"))))

(define (pretty-print-citadel-result data result)
  (if (= (length result) 8)
      (format "~a: ~a @ ~akm, belonging to ~a"
	      (parse-solarsystem :name (third result))
	      (hash-ref (dscan-proximity (citadel? data)) 'type)
	      (hash-ref (dscan-proximity (citadel? data)) 'distance)
	      (if (sql-null? (sixth result))
		  "-"
		  (string-append
		   (let ([corporation (parse-corporation (sixth result))])
		     (if (false? corporation)
			 "? "
			 (vector-ref corporation 2)))
		   "["
		   (sixth result)
		   "]")))
      "No valid location found"))

(define location (make-parameter ""))

(define (location? query)
  (if (false? (query-maybe-row sqlc "SELECT * FROM mapDenormalize WHERE itemName = ?" query))
      #f
      query))

;; Moon database

(define regions
  '("A-R00001" "A-R00002" "A-R00003" "Aridia" "B-R00004" "B-R00005" "B-R00006" "B-R00007" "B-R00008" "Black Rise" "Branch" "C-R00009" "C-R00010" "C-R00011" "C-R00012" "C-R00013" "C-R00014" "C-R00015" "Cache" "Catch" "Cloud Ring" "Cobalt Edge" "Curse" "D-R00016" "D-R00017" "D-R00018" "D-R00019" "D-R00020" "D-R00021" "D-R00022" "D-R00023" "Deklein" "Delve" "Derelik" "Detorid" "Devoid" "Domain" "E-R00024" "E-R00025" "E-R00026" "E-R00027" "E-R00028" "E-R00029" "Esoteria" "Essence" "Etherium Reach" "Everyshore" "F-R00030" "Fade" "Feythabolis" "Fountain" "G-R00031" "Geminate" "Genesis" "Great Wildlands" "H-R00032" "Heimatar" "Immensea" "Impass" "Insmother" "K-R00033" "Kador" "Khanid" "Kor-Azor" "Lonetrek" "Malpais" "Metropolis" "Molden Heath" "Oasa" "Omist" "Outer Passage" "Outer Ring" "Paragon Soul" "Period Basis" "Perrigen Falls" "Placid" "Providence" "Pure Blind" "Querious" "Scalding Pass" "Sinq Laison" "Solitude" "Stain" "Syndicate" "Tash-Murkon" "Tenal" "Tenerifis" "The Bleak Lands" "The Citadel" "The Forge" "The Kalevala Expanse" "The Spire" "Tribute" "Vale of the Silent" "Venal" "Verge Vendor" "Wicked Creek"))

(define (region? query) (findf (lambda (str) (equal? (string-downcase query) (string-downcase str))) regions))

(define (query-regions lst) (cond [(empty? lst) null]
				  [(and (not (empty? lst)) (string-empty? (car lst))) null]
				  [else (filter-map region? (string-split (car lst) ","))]))

(define (sql-moon-region-towers param)
  (map vector->list (query-rows sqlc "SELECT * FROM moonScanView WHERE regionName LIKE ?" param)))

(define (sql-moon-get-towers)
  (query-rows sqlc "SELECT * FROM moonScanView"))

(define (user-filter-regions lst #:filter-function filter-function #:function function)
  (cond
   [(not (empty? (query-regions lst)))
    (append-map (lambda (region) (filter-function region))
		(query-regions lst))]
   [else function]))

;; Servlet

(define (exec-report req)
  (send/back
   (response/output
    (lambda (port)
      (output-xml (doctype 'html) port)
      (output-xml
       (html
	(output:create-html-head #:title "Dashboard" #:tablesorter #f)
	(body
	 (div 'id: "content"
	      (h1 "Dashboard")
	      (form 'action: (main-url exec-result) 'method: "POST" 'target: "_self" 'id: "main" 'novalidate: #t
		    (fieldset
		     (legend "D-Scan reporting")
		     (br)
		     "Corporation Ticker: "
		     (input 'type: "text" 'name: "corporation" 'maxlength: "5" 'size: "5" 'required: #f 'autocomplete: "on" 'style: "margin-right:1em;")
		     "Alliance Ticker: "
		     (input 'type: "text" 'name: "alliance" 'maxlength: "5" 'size: "5" 'required: #f 'autocomplete: "on")
		     (br)
		     (br)
		     (textarea 'name: "dscan" 'rows: "20" 'cols: "50")
		     (br)
		     (br)
		     (input 'type: "checkbox" 'name: "checkbox" 'value: "chechbox" "Checkbox")
		     (br)
		     ;; (input 'type: "checkbox" 'name: "empty" 'value: "empty" "Empty (no tower)")
		     ;; (br)
		     ;; (p "Note: only enter a location if no celestial appears on D-Scan")
		     ;; "Location: "
		     ;; (input 'type: "text" 'name: "location" 'required: #f 'autocomplete: "on" 'style: "margin-right:1em;")
		     ;; (br)
		     (br)
		     (input 'type: "submit" 'value: "Submit"))))))
       port)))))


(define (exec-result req) 
  (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
  (define form-data (form-urlencoded->alist post-data))

  (define corporation    (cdr (assq 'corporation form-data)))
  (define alliance (cdr (assq 'alliance form-data)))
  (define dscan (cdr (assq 'dscan form-data)))
  ;; (define location (cdr (assq 'location form-data)))

  (define data
    (dscan-list->hash
     (dscan-normalise-distance
      (dscan-raw->list dscan))))

  (define moonscan-result
    (cond
     [(or (string-empty? dscan)
	  (false? (dscan-proximity (tower? data)))
	  (false? (dscan-proximity (moon? data))))
      #f]
     [else (let ([result (moonscan data #:corporation corporation #:alliance alliance)])
	     (if (false? result)
		 #f
		 result))]))

  (define citadelscan-result
    (cond
     [(or (string-empty? dscan)
	  (false? (dscan-proximity (citadel? data))))
      #f]
     [else (let ([result (citadelscan data #:corporation corporation #:alliance alliance #:location (location))])
	     (if (false? result)
		 #f
		 result))]))

  (when (not (false? moonscan-result))
    (sql-moon-update-scan (list moonscan-result)))

  ;; (when (and (not (false? citadelscan-result))
  ;; 	       (not (null? guess-or-location? data (location))))
  ;;   (sql-citadel-update-scan (list citadelscan-result)))

  (send/back
   (response/output
    (lambda (port)
      (output-xml (doctype 'html) port)
      (output-xml
       (html
	(output:create-html-head #:title "Dashboard Scan Result" #:tablesorter #f)
	(body
	 (div 'id: "content"
	      (h1 (pretty-print-location (guess-or-location? data (location))))
	      (b "Scan Result: ")
	      (cond
	       [(not (false? moonscan-result)) (pretty-print-moon-result data moonscan-result)]
	       [(not (false? citadelscan-result)) (pretty-print-citadel-result data citadelscan-result)]
	       [else "No structure found in close proximity"])
	      (br)
	      (br)
	      (hr)
	      (form 'action: (main-url exec-report) (input 'type: "submit" 'value: "Return to Dashboard")))))
       port)))))

(define (exec-moon-database req)
  (define (get-regions req)
    (match
      (bindings-assq
       #"region"
       (request-bindings/raw req))
      [(? binding:form? b)
       (list
	(bytes->string/utf-8
	 (binding:form-value b)))]
      [_ null]))

  (define filter_region (get-regions req))

  (send/back
   (response/output
    (lambda (port)
      (output-xml (doctype 'html) port)
      (output-xml
       (html
	(output:create-html-head
	 #:title "Moon Scan Data"
	 #:tablesorter #t
	 #:sort-column 0
	 (list (literal (style/inline 'type: "text/css" "tr > td[class=\"LOLTX\"], tr > td[class=\"OHGOD\"] { background-color: #4D6EFF; color: white; }"))
	       (literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
	       (literal (style/inline 'type: "text/css" "td { white-space: normal; }"))
	       (literal (style/inline 'type: "text/css" "select { margin-right: 0.5em; }"))
	       (literal (style/inline 'type: "text/css" "span { margin: 0 .25em; }"))
	       (literal (style/inline 'type: "text/css" "tr.offline, span.offline { color: gray; }"))
	       (literal (style/inline 'type: "text/css" "tr.rescan, span.rescan { background-color: orange; }"))))
	(body
	 (output:create-region-filter regions)
	 (div 'id: "content"
	      (h1 "Moon Scan Data")
	      (output:create-html-hint (output:create-html-legend))
	      (output:create-html-hint (format "Results filtered for: Region (~a)"
					       (string-join (query-regions filter_region)  "|")))
	      (output:create-html-hint :tablesorter)
	      (output:create-html-table #:ticker->class #t
					#:drop-right 2
					#:head (list "Region" "Constellation" "System" "Planet" "Moon" "CT"
						     "Alliance" "AT" "Corporation" "Date" "Tower" "Goo")
					(user-filter-regions filter_region
							     #:filter-function sql-moon-region-towers
							     #:function (map vector->list (sql-moon-get-towers))))
	      (output:create-html-hint :updated))))
       port)))))

(define (exec-timers req)
  
  (define (get-regions req)
    (match
      (bindings-assq
       #"region"
       (request-bindings/raw req))
      [(? binding:form? b)
       (list
	(bytes->string/utf-8
	 (binding:form-value b)))]
      [_ null]))

  (define filter_region (get-regions req))
  
  (send/back
   (response/output
    (lambda (port)
      (output-xml (doctype 'html) port)
      (output-xml
       (html
	(output:create-html-head #:title "Fuzzysov Timer Board" #:sort-column 5
				 (list
				  (literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
				  (literal (style/inline 'type: "text/css" "select { margin-right: 0.5em; }"))))
	(body
	 (output:create-region-filter regions)
	 (h1 "Fuzzysov Timer Board")
	 (output:create-html-hint "Note: Sovereignty data is updated every 10 minutes")
	 (output:create-html-hint :tablesorter)
	 (output:create-html-table #:id "timers"
				   #:head (list "Alliance" "Structure" "System"
						"Constellation" "Region" "Date")
				   (user-filter-regions filter_region
							#:filter-function timerboard-query-region
							#:function (timerboard-query)))
	 (output:create-html-hint :updated)))
       port)))))

(serve/servlet main
	       #:stateless? #t
	       #:port 8000
	       #:command-line? #t
	       #:banner? #t
	       #:servlet-regexp #rx""
	       #:servlet-current-directory webroot
	       #:mime-types-path (build-path "/etc/mime.types"))