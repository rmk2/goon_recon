#! /usr/bin/env racket
#lang racket

(require web-server/servlet
         web-server/servlet-env
	 web-server/configuration/responders)

(require racket/random)
(require grommet/crypto/hash/sha256)
(require (only-in openssl/sha1
		  bytes->hex-string
		  hex-string->bytes))

(require (prefix-in html: (only-in scribble/html/xml
				   make-element)))

(require eve)

;; URL dispatch

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("setup") exec-setup]
   [("setup") #:method "post" exec-setup]
   [("setup" (string-arg)) #:method "post" exec-setup-post]
   [("join" (string-arg)) exec-join]
   [("report") exec-report]
   [("report") #:method "post" exec-report-post]
   [("parse") #:method "post" exec-parse]
   [("result" (string-arg)) exec-result-pre]
   [("result" (string-arg)) #:method "post" exec-result-post]))

;; Structs

(struct sovNode (id name type target sessionid [status #:mutable] [datetime #:mutable]) #:transparent)

(struct sovSession (sessionid region constellation system datetime) #:transparent)

(struct sovName (sessionid name datetime) #:transparent)

(struct sovMaster (id constellation datetime) #:transparent)

(struct sovSlave (sessionid masterid datetime) #:transparent)

(struct sovAuth (id password salt datetime) #:transparent)

;; SQL

;; Create tables/views

(define (sql-sov-create-session-raw)
  (if (table-exists? sqlc "sovSessionRaw")
      #t
      (query-exec sqlc "CREATE TABLE sovSessionRaw ( sessionID VARCHAR(64) NOT NULL, regionID INT, constellationID INT, solarSystemID INT NOT NULL, datetime DATETIME, PRIMARY KEY ( sessionID ) )")))

(define (sql-sov-create-session-view)
  (if (table-exists? sqlc "sovSessionView")
      #t
      (query-exec sqlc "CREATE VIEW sovSessionView AS SELECT sessionID,regionName,constellationName,solarSystemName,datetime FROM sovSessionRaw AS sov LEFT JOIN mapRegions ON sov.regionID = mapRegions.regionID LEFT JOIN mapConstellations ON sov.constellationID = mapConstellations.constellationID LEFT JOIN mapSolarSystems ON sov.solarSystemID = mapSolarSystems.solarSystemID")))

(define (sql-sov-create-nodes-raw)
  (if (table-exists? sqlc "sovNodesRaw")
      #t
      (query-exec sqlc "CREATE TABLE sovNodesRaw ( nodeID VARCHAR(64) NOT NULL, nodeName VARCHAR(255), nodeType VARCHAR(255) NOT NULL, nodeTargetID INT, sessionID VARCHAR(64) NOT NULL, nodeStatus VARCHAR(20), datetime DATETIME, PRIMARY KEY ( nodeID ), INDEX ( sessionID ) )")))

(define (sql-sov-create-nodes-view)
  (if (table-exists? sqlc "sovNodesView")
      #t
      (query-exec sqlc "CREATE VIEW sovNodesView AS SELECT nodeID,nodeName,nodeType,solarSystemName AS nodeTargetName,sessionID,nodeStatus,datetime FROM sovNodesRaw LEFT JOIN mapSolarSystems ON sovNodesRaw.nodeTargetID = mapSolarSystems.solarSystemID")))

(define (sql-sov-create-session-names)
  (if (table-exists? sqlc "sovSessionNames")
      #t
      (query-exec sqlc "CREATE TABLE sovSessionNames ( sessionID VARCHAR(64) NOT NULL, scoutName VARCHAR(255), datetime DATETIME, PRIMARY KEY ( sessionID ) )")))

(define (sql-sov-create-session-master)
  (if (table-exists? sqlc "sovSessionMasters")
      #t
      (query-exec sqlc "CREATE TABLE sovSessionMasters ( masterID VARCHAR(64) NOT NULL, constellationID INT NOT NULL, datetime DATETIME, PRIMARY KEY ( masterID ) )")))

(define (sql-sov-create-session-slaves)
  (if (table-exists? sqlc "sovSessionSlaves")
      #t
      (query-exec sqlc "CREATE TABLE sovSessionSlaves ( sessionID VARCHAR(64) NOT NULL, masterID VARCHAR(64) NOT NULL, datetime DATETIME, PRIMARY KEY ( sessionID ) )")))

(define (sql-sov-create-session-auth-scout)
  (if (table-exists? sqlc "sovSessionAuthMasters")
      #t
      (query-exec sqlc "CREATE TABLE sovSessionAuthMasters ( masterID VARCHAR(64) NOT NULL, password VARCHAR(64), salt VARCHAR(64), datetime DATETIME, PRIMARY KEY ( masterID ) )")))

(define (sql-sov-create-session-auth-admin)
  (if (table-exists? sqlc "sovSessionAuthAdmins")
      #t
      (query-exec sqlc "CREATE TABLE sovSessionAuthAdmins ( sessionID VARCHAR(64) NOT NULL, password VARCHAR(64), salt VARCHAR(64), datetime DATETIME, PRIMARY KEY ( sessionID ) )")))

;; Update tables

(define (sql-sov-update-session-raw lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO sovSessionRaw VALUES (?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE regionID=?,constellationID=?,solarSystemID=?,datetime=?"
		     (sovSession-sessionid x)
		     (sovSession-region x)
		     (sovSession-constellation x)
		     (sovSession-system x)
		     (sovSession-datetime x)
		     (sovSession-region x)
		     (sovSession-constellation x)
		     (sovSession-system x)
		     (sovSession-datetime x)))
	    lst))

(define (sql-sov-update-nodes lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO sovNodesRaw VALUES (?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE sessionID=?,nodeStatus=?,datetime=?"
		     (sovNode-id x)
		     (sovNode-name x)
		     (sovNode-type x)
		     (sovNode-target x)
		     (sovNode-sessionid x)
		     (sovNode-status x)
		     (sovNode-datetime x)
		     (sovNode-sessionid x)
		     (sovNode-status x)
		     (sovNode-datetime x)))
	    lst))

(define (sql-sov-update-session-names lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO sovSessionNames VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE scoutName=?,datetime=?"
		     (sovName-sessionid x)
		     (sovName-name x)
		     (sovName-datetime x)
		     (sovName-name x)
		     (sovName-datetime x)))
	    lst))

(define (sql-sov-update-session-masters lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO sovSessionMasters VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE constellationID=?,datetime=?"
		     (sovMaster-id x)
		     (sovMaster-constellation x)
		     (sovMaster-datetime x)
		     (sovMaster-constellation x)
		     (sovMaster-datetime x)))
	    lst))

(define (sql-sov-update-session-slaves lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO sovSessionSlaves VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE masterID=?,datetime=?"
		     (sovSlave-sessionid x)
		     (sovSlave-masterid x)
		     (sovSlave-datetime x)
		     (sovSlave-masterid x)
		     (sovSlave-datetime x)))
	    lst))

(define (sql-sov-update-auth-masters lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO sovSessionAuthMasters VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE password=?,salt=?,datetime=?"
		     (sovAuth-id x)
		     (sovAuth-password x)
		     (sovAuth-salt x)
		     (sovAuth-datetime x)
		     (sovAuth-password x)
		     (sovAuth-salt x)
		     (sovAuth-datetime x)))
	    lst))

(define (sql-sov-update-auth-admins lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO sovSessionAuthAdmins VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE password=?,salt=?,datetime=?"
		     (sovAuth-id x)
		     (sovAuth-password x)
		     (sovAuth-salt x)
		     (sovAuth-datetime x)
		     (sovAuth-password x)
		     (sovAuth-salt x)
		     (sovAuth-datetime x)))
	    lst))

;; Get data from tables

(define (sql-sov-get-session session)
  (query-maybe-row sqlc "SELECT sessionID,regionName,constellationName,solarSystemName,datetime FROM sovSessionView WHERE sessionID = ?" session))

(define (sql-sov-get-session->struct session)
  (sql-parse->struct (sql-sov-get-session session) #:struct sovSession))

(define-syntax sql-sov-get-nodes
  (syntax-rules (:hide-inactive :show-all)
    ((_ :hide-inactive session)
     (query-rows sqlc "SELECT nodeID,nodeName,nodeType,nodeTargetName,sessionID,nodeStatus,datetime FROM sovNodesView WHERE sessionID = ? AND nodeStatus != 'DELETED'" session))
    ((_ :show-all session)
     (query-rows sqlc "SELECT nodeID,nodeName,nodeType,nodeTargetName,sessionID,nodeStatus,datetime FROM sovNodesView WHERE sessionID = ?" session))
    ((_ session) (sql-sov-get-nodes :hide-inactive session))))

(define (sql-sov-get-nodes->struct session)
  (map (lambda (node) (sql-parse->struct node #:struct sovNode))
       (sql-sov-get-nodes session)))

(define (sql-sov-get-nodes->list session)
  (map (lambda (node) (list (sovNode-id node)
			    (sovNode-name node)
			    (sovNode-type node)
			    (sovNode-target node)))
       (sql-sov-get-nodes->struct session)))

(define (sql-sov-get-nodes->table session)
  (map (lambda (node)
	 (let* ([status (sovNode-status node)]
		[deleted? (if (equal? status "DELETED") #t #f)]
		[entosis? (if (equal? status "ENTOSIS") #t #f)]
		[idle? (if (not (equal? status "ENTOSIS")) #t #f)])
	   (list
	    (sovNode-name node)
	    (sovNode-type node)
	    (sovNode-target node)
	    (sovNode-datetime node)
	    (div 'class: "status"
		 (input 'type: "radio" 'form: "main" 'name: (sovNode-id node) 'value: "idle" 'checked: idle? "Idle")
		 (input 'type: "radio" 'form: "main" 'name: (sovNode-id node) 'value: "entosis" 'checked: entosis? "Entosis"))
	    (input 'type: "checkbox" 'form: "main" 'name: (sovNode-id node) 'value: "delete" "X"))))
       (sql-sov-get-nodes->struct session)))

(define (sql-sov-get-node-data node)
  (query-maybe-row sqlc "SELECT nodeID,nodeName,nodeType,nodeTargetID,sessionID,nodeStatus,datetime FROM sovNodesRaw WHERE nodeID = ?" node))

(define (sql-sov-get-name session)
  (query-maybe-row sqlc "SELECT sessionID,scoutName,datetime FROM sovSessionNames WHERE sessionID = ?" session))

(define (sql-sov-get-count session)
  (let ([poll (query-maybe-value sqlc "SELECT COUNT(nodeID) FROM sovNodesView WHERE sessionID = ? AND nodeStatus != 'DELETED' GROUP BY sessionID" session)])
    (if (false? poll) 0 poll)))

(define (sql-sov-get-master master)
  (query-maybe-row sqlc "SELECT masterID,constellationID,datetime FROM sovSessionMasters WHERE masterID = ?" master))

(define (sql-sov-get-master-data master)
  (query-rows sqlc (string-append "SELECT session.solarSystemName,node.nodeName,node.nodeType,"
				  "node.nodeTargetName,node.nodeStatus,node.datetime,name.scoutName,"
				  "slave.sessionID "
				  "FROM sovSessionSlaves AS slave "
				  "RIGHT JOIN sovNodesView AS node ON slave.sessionID = node.sessionID "
				  "RIGHT JOIN sovSessionNames AS name ON slave.sessionID = name.sessionID "
				  "RIGHT JOIN sovSessionView AS session ON slave.sessionID = session.sessionID "
				  "WHERE slave.masterID = ?")
	      master))

;; SQL helper functions

(define (sql-sov-create-session-id #:length [length 32])
  (bytes->hex-string (subbytes (sha256-bytes (crypto-random-bytes 128)) 0 (/ length 2))))

(define (sql-sov-get-regions)
  (query-list sqlc "SELECT regionName FROM mapRegions WHERE factionID IS NULL AND x < 5.0e18 AND regionName != 'UUA-F4' ORDER BY regionName"))

(define (sql-sov-cleanup-tables)
  (begin
    (query-exec sqlc "TRUNCATE sovNodesRaw")
    (query-exec sqlc "TRUNCATE sovSessionRaw")
    (query-exec sqlc "TRUNCATE sovSessionNames")
    (query-exec sqlc "TRUNCATE sovSessionMasters")
    (query-exec sqlc "TRUNCATE sovSessionSlaves")))

;; Get systems data for a given constellation

(define (constellation->systems input)
  (let ([maybe-constellation
	 (match input
	   [(? number? in)
	    (query-maybe-value sqlc "SELECT constellationID FROM mapConstellations WHERE constellationID = ?" in)]
	   [(? string? in)
	    (query-maybe-value sqlc "SELECT constellationID FROM mapConstellations WHERE constellationName = ?" in)]
	   [else #f])])
    (cond [(not (false? maybe-constellation))
	   (query-list sqlc "SELECT solarSystemName FROM mapSolarSystems WHERE constellationID = ?" maybe-constellation)]
	  [else null])))

(define (constellation->system-data constellation)
  (query-rows sqlc "SELECT solarSystemName,solarSystemID FROM mapSolarSystems WHERE constellationID = ?" constellation))

;; Get regionName + constellationName for a given constellation

(define (sql-constellation->names constellation)
  (let ([maybe-data
	 (match constellation
	   [(? number? in)
	    (query-maybe-row sqlc "SELECT regionName,constellationName FROM mapConstellations LEFT JOIN mapRegions ON mapConstellations.regionID = mapRegions.regionID WHERE constellationID = ?" in)]
	   [(? string? in)
	    (query-maybe-row sqlc "SELECT regionName,constellationName FROM mapConstellations LEFT JOIN mapRegions ON mapConstellations.regionID = mapRegions.regionID WHERE constellationName = ?" in)]
	   [else #f])])
    (cond [(not (false? maybe-data))
	   (vector->list maybe-data)]
	  [else (list "Unknown" "Unknown")])))

;; Get constellations for a given region

(define (region->constellations input)
  (match input
    [(? number? in)
     (query-list sqlc "SELECT constellationName FROM mapConstellations WHERE regionID = ?" in)]
    [(? string? in)
     (query-list sqlc "SELECT constellationName FROM mapConstellations LEFT JOIN mapRegions ON mapConstellations.regionID = mapRegions.regionID WHERE regionName = ?" in)]
    [else null]))

;; Extract POST data from HTTP requests

(define (extract-post-data req query)
  (match
    (bindings-assq
     query
     (request-bindings/raw req))
    [(? binding:form? b)
     (bytes->string/utf-8
      (binding:form-value b))]
    [_ null]))

;; Extract and prepare node data from raw POST bindings

(define (combine-node-data req)
  (let* ([bind-filter (filter-map (lambda (bind)
				    (match bind
				      [(binding (not #"region" #"constellation" #"system" #"count" #"sessionID")) bind]
				      [else #f]))
				  (request-bindings/raw req))]
	 [bind-sort (sort bind-filter bytes<? #:key binding-id)])
    (map (lambda (l) (let* ([id (car l)] [data (bindings-assq-all id bind-sort)])
		       (list (bytes->string/utf-8 id)
			     (map (lambda (value) (bytes->string/utf-8 (binding:form-value value))) data))))
	 (count-duplicates (map binding-id bind-sort)))))

(define (node-data->list lst)
  (map (lambda (node)
	 (let ([id (car node)] [inner (cadr node)])
	   (append (list id) inner)))
       lst))

;; Pages

(define (exec-setup req [constellation ""])
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (head (title "Fuzzysov Node Reporting")
	       (style/inline 'type: "text/css" ".form-description:after { content: ':'; }")
	       (style/inline 'type: "text/css" ".form-entry { display: flex; flex-flow: column wrap; margin-bottom: 1em; }")
	       (style/inline 'type: "text/css" ".form-password { margin-bottom: 1em; padding: 1em 1em 0; border: 1px solid lightgrey; }")
	       (literal (style/inline 'type: "text/css" " input[type='password'], select { width: 100%; }"))
	       (style/inline 'type: "text/css" ".form-error { margin-bottom: 1em; color: indianred; }")
	       (style/inline 'type: "text/css" ".subtitle { margin-bottom: 1em; font-weight: inherit; font-size: large; }")
	       (style/inline 'type: "text/css" "#content { display: flex; flex-flow: column wrap; align-items: flex-start;  margin: 0 2em; }")
	       (style/inline 'type: "text/css" "#main { border: 1px solid black; background-color: whitesmoke; padding: 1em; }"))
	 (body
	  (div 'id: "content"
	       (h1 "Fuzzysov Node Reporting")
	       (div 'class: "subtitle" "Scout Reporting Setup")
	       (form 'id: "main" 'name: "main" 'method: "POST" 'action: (format "/setup/~a" masterID)
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Region")
			  (div 'class: "form-field"
			       (select 'name: "region" 'form: "inner" 'onchange: "this.form.submit()"
				       (map (lambda (name) (let ([selected? (if (equal? region name) #t #f)])
							     (option 'selected: selected? 'value: name name)))
					    (sql-sov-get-regions)))))
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Constellation")
			  (div 'class: "form-field"
			       (select 'name: "constellation"
				       (map (lambda (name) (option 'value: name name)) (region->constellations region)))))
		     (input 'type: "submit" 'value: "Submit"))
	       (form 'id: "inner" 'name: "inner" 'method: "POST"))))
	out))))

  (define region
    (let ([maybe-region (extract-post-data req #"region")])
      (if (null? maybe-region)
	  "Branch"
	  maybe-region)))

  (define masterID (sql-sov-create-session-id #:length 32))

  (send/back response-generator))

(define (exec-setup-post req [master ""])
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (head (title "Fuzzysov Node Reporting")
	       (style/inline 'type: "text/css" ".form-description:after { content: ':'; }")
	       (style/inline 'type: "text/css" ".form-entry { display: flex; flex-flow: column wrap; margin-bottom: 1em; }")
	       (style/inline 'type: "text/css" ".form-password { margin-bottom: 1em; padding: 1em 1em 0; border: 1px solid lightgrey; }")
	       (literal (style/inline 'type: "text/css" " input[type='password'], select { width: 100%; }"))
	       (style/inline 'type: "text/css" ".form-error { margin-bottom: 1em; color: indianred; display: none; }")
	       (style/inline 'type: "text/css" ".subtitle { margin-bottom: 1em; font-weight: inherit; font-size: large; }")
	       (style/inline 'type: "text/css" "#content { display: flex; flex-flow: column wrap; align-items: flex-start;  margin: 0 2em; }")
	       (style/inline 'type: "text/css" "#main { border: 1px solid black; background-color: whitesmoke; padding: 1em; }")
	       (script 'type: "text/javascript" (literal "function checkPW(form,e1,e2,error) { var err = document.getElementById(error); if (form.elements[e1].value == form.elements[e2].value ) { err.style.display = 'none'; form.elements['submit'].disabled = false } else { err.style.display = 'block'; form.elements['submit'].disabled = true } };")))
	 (body
	  (div 'id: "content"
	       (h1 "Fuzzysov Node Reporting")
	       (let* ([map-data (sql-constellation->names constellation)]
		      [region-name (first map-data)]
		      [constellation-name (second map-data)])
		 (div 'class: "subtitle" (format "~a » ~a" region-name constellation-name)))
	       (p (format "Setup successful for masterID ~a and constellation ~a!" masterID constellation))
	       (p "Please give the following link to your scouts: "
		  (a 'target: "_blank" 'rel: "noopener noreferrer" 'href: (format "/join/~a" masterID) join-link))
	       (p "Please use the following link to get to your admin page: "
		  (a 'target: "_blank" 'rel: "noopener noreferrer" 'href: (format "/admin/~a" adminID) admin-link))
	       (form 'id: "main" 'name: "main" 'method: "POST"
		     (div 'class: "form-password"
		     	  ;; (if (false? pass-admin-match?) (div 'class: "form-error" "Passwords do not match") null)
			  ;; (div 'class: "form-error" 'id: "admin-error" "Passwords do not match")
		     	  (div 'class: "form-entry"
		     	       (div 'class: "form-description" "Admin password")
		     	       (div 'class: "form-field"
		     		    (input 'type: "password" 'name: "pass-admin" 'id: "pass-admin")))
		     	  (div 'class: "form-entry"
		     	       (div 'class: "form-description" "Confirm password")
		     	       (div 'class: "form-field"
		     		    (input 'type: "password"
					   'name: "pass-admin-confirm"
					   'id: "pass-admin-confirm"
					   'onChange: "checkPW(main,'pass-admin',1,'admin-error');")))
			  (div 'class: "form-error" 'id: "admin-error" "Entries don't match"))
		     (div 'class: "form-password"
		     	  ;; (if (false? pass-scout-match?) (div 'class: "form-error" "Passwords do not match") null)
			  ;; (div 'class: "form-error" 'id: "scout-error" "Passwords do not match")
		     	  (div 'class: "form-entry"
		     	       (div 'class: "form-description" "Scout password")
		     	       (div 'class: "form-field"
		     		    (input 'type: "password" 'name: "pass-scout" 'id: "pass-scout")))
		     	  (div 'class: "form-entry"
		     	       (div 'class: "form-description" "Confirm password")
		     	       (div 'class: "form-field"
		     		    (input 'type: "password"
					   'name: "pass-scout-confirm"
					   'id: "pass-scout-confirm"
					   'onChange: "checkPW(main,2,3,'scout-error');")))
			  (div 'class: "form-error" 'id: "scout-error" "Entries don't match"))
		     (input 'type: "hidden" 'name: "adminID" 'value: adminID 'readonly: #t)
		     (input 'type: "hidden" 'name: "constellation" 'value: constellation 'readonly: #t)
		     ;; (input 'type: "submit" 'id: "submit" 'form: "main" 'disabled: block-submit? 'value: "Submit")))))
		     (input 'type: "submit" 'id: "submit" 'form: "main" 'value: "Submit")))))
	out))))

  (define-values (masterID adminID constellation)
    (let* ([maybe-id (sql-sov-get-master master)]
  	   [maybe-admin (extract-post-data req #"adminID")]
  	   [maybe-constellation (extract-post-data req #"constellation")]
  	   [maybe-master (if (false? maybe-id) #f (sql-parse->struct maybe-id #:struct sovMaster))])
      (values
       (if (false? maybe-master)
  	   ;; (response/full 404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE null null)
	   master
  	   (sovMaster-id maybe-master))
       (if (null? maybe-admin)
  	   (sql-sov-create-session-id #:length 32)
  	   maybe-admin)
       (if (and (null? maybe-constellation) (not (false? maybe-master)))
  	   (parse-constellation :name (sovMaster-constellation maybe-master))
  	   maybe-constellation))))

  ;; (define-values (pass-admin pass-admin-confirm pass-scout pass-scout-confirm)
  ;;   (values
  ;;    (extract-post-data req #"pass-admin")
  ;;    (extract-post-data req #"pass-admin-confirm")
  ;;    (extract-post-data req #"pass-scout")
  ;;    (extract-post-data req #"pass-scout-confirm")))

  ;; (define pass-admin-match? (if (equal? pass-admin pass-admin-confirm) #t #f))
  ;; (define pass-scout-match? (if (equal? pass-scout pass-scout-confirm) #t #f))
  ;; (define block-submit? (if (or (false? pass-admin-match?) (false? pass-scout-match?)) #t #f))

  (define join-link (format "~a/join/~a" (header-value (headers-assq* #"host" (request-headers/raw req))) masterID))
  (define admin-link (format "~a/admin/~a" (header-value (headers-assq* #"host" (request-headers/raw req))) adminID))

  (sql-sov-update-session-masters
   (list (apply sovMaster (list masterID
  				(parse-constellation :id constellation)
  				(srfi-date->sql-timestamp (current-date))))))

  ;; (when (and pass-admin-match? (false? block-submit?) (string? pass-admin))
  ;;   (sql-sov-update-auth-admins (list (struct-copy scrypt-hash
  ;; 						   (auth:scrypt-input->hash (string->bytes/utf-8 pass-admin) #:length 32)
  ;; 						   [user adminID]))))
  
  ;; (when (and pass-scout-match? (false? block-submit?) (string? pass-scout))
  ;;   (sql-sov-update-auth-masters (list (struct-copy scrypt-hash
  ;; 						    (auth:scrypt-input->hash (string->bytes/utf-8 pass-scout) #:length 32)
  ;; 						    [user masterID]))))

  (send/back response-generator))

(define (exec-join req [master ""])
  (let ([maybe-id (sql-sov-get-master master)])
    (if (false? maybe-id)
	(response/full 404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE null null)
	(let ([master (sql-parse->struct maybe-id #:struct sovMaster)])
	  (exec-report (struct-copy request req
				    [headers/raw (append
						  (list (make-header #"x-masterid"
								     (string->bytes/utf-8 (sovMaster-id master))))
						  (request-headers/raw req))]))))))

(define (exec-report req)
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (head (title "Fuzzysov Node Reporting")
	       (style/inline 'type: "text/css" ".form-description:after { content: ':'; }")
	       (style/inline 'type: "text/css" ".form-entry { display: flex; flex-flow: column wrap; margin-bottom: 1em; }")
	       (style/inline 'type: "text/css" ".subtitle { margin-bottom: 1em; font-weight: inherit; font-size: large; }")
	       (style/inline 'type: "text/css" "#content { display: flex; flex-flow: column wrap; align-items: flex-start;  margin: 0 2em; }")
	       (style/inline 'type: "text/css" "form { border: 1px solid black; background-color: whitesmoke; padding: 1em; }"))
	 (body
	  (div 'id: "content"
	       (h1 "Fuzzysov Node Reporting")
	       (let* ([map-data (sql-constellation->names constellation)]
		      [region-name (first map-data)]
		      [constellation-name (second map-data)])
		 (div 'class: "subtitle" (format "~a » ~a" region-name constellation-name)))
	       (form 'method: "POST" 'action: "/report"
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Scout Name")
			  (div 'class: "form-field" (input 'type: "text" 'name: "scout" 'required: #t)))
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "System")
			  (div 'class: "form-field"
			       (select 'name: "system" 'required: #t
				       (map (lambda (sys) (option 'value: sys sys)) (constellation->systems constellation)))))
		     (div 'class: "form-entry"
			  (div 'class: "form-description" "Node Count")
			  (div 'class: "form-field" (input 'type: "number" 'name: "count" 'min: 0 'max: 99 'value: 0)))
		     (input 'type: "hidden" 'name: "masterID" 'value: masterID 'readonly: #t)
		     (input 'type: "submit")))))
	out))))

  (define masterID
    (let ([maybe-id (headers-assq* #"x-masterid" (request-headers/raw req))])
      (if (false? maybe-id)
	  ""
	  (bytes->string/utf-8 (header-value maybe-id)))))

  (define constellation
    (let ([maybe-id (sql-sov-get-master masterID)])
      (if (false? maybe-id)
	  ""
	  (sovMaster-constellation (sql-parse->struct maybe-id #:struct sovMaster)))))

  (send/back response-generator))

(define (exec-report-post req)
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (head (title "Fuzzysov Node Reporting")
	       (style/inline 'type: "text/css" ".form-entry { display: flex; flex-flow: row wrap; margin-bottom: 1em; }")
	       (style/inline 'type: "text/css" "#content { display: flex; flex-flow: column wrap; align-items: flex-start;  margin: 0 2em; }")
	       (style/inline 'type: "text/css" "form { border: 1px solid black; background-color: whitesmoke; padding: 1em; }"))
	 (body
	  (div 'id: "content"
	       (h1 (format "~a » ~a » ~a" region constellation system))
	       (form 'method: "POST" 'action: "parse"
		     (if (positive? (string->number count))
			 (map (lambda (x)
				(let ([id (sql-sov-create-session-id #:length 16)])
				  (div 'class: "form-entry"
				       (input 'type: "text" 'name: id 'placeholder: "Node name" 'style: "margin-right: 0.75em;")
				       (select 'name: id 'style: "margin-right: 0.75em;"
					       (map (lambda (opt) (option 'value: opt opt)) '("IHub" "Station" "TCU")))
				       (select 'name: id
					       (map (lambda (opt) (option 'value: opt opt)) (constellation->systems constellation))))))
			      (range (string->number count)))
			 (list
			  (div 'class: "form-entry" "No initial nodes have been submitted.")
			  (div 'class: "form-entry" "New nodes can be added on the next page.")))
		     (input 'type: "hidden" 'name: "region" 'value: region 'readonly: #t)
		     (input 'type: "hidden" 'name: "constellation" 'value: constellation 'readonly: #t)
		     (input 'type: "hidden" 'name: "system" 'value: system 'readonly: #t)
		     (input 'type: "hidden" 'name: "count" 'value: count 'readonly: #t)
		     (input 'type: "hidden" 'name: "sessionID" 'value: sessionID 'readonly: #t)
		     (if (positive? (string->number count))
			 (input 'type: "submit")
			 (input 'type: "submit" 'value: "Continue"))))))
	out))))

  (define-values (system count scout masterID)
    (values
     (extract-post-data req #"system")
     (extract-post-data req #"count")
     (extract-post-data req #"scout")
     (extract-post-data req #"masterID")))

  (define-values (region constellation location sessionID)
    (let ([location (sql-parse->struct (parse-map system) #:struct mapDenormalize)])
      (values
       (parse-region :name (mapDenormalize-region location))
       (parse-constellation :name (mapDenormalize-constellation location))
       location
       (let ([maybe-id (extract-post-data req #"sessionID")])
	 (if (null? maybe-id) (sql-sov-create-session-id #:length 32) maybe-id)))))

  (let ([timestamp (srfi-date->sql-timestamp (current-date))])
    (sql-sov-update-session-raw
     (list (apply sovSession (list sessionID
				   (mapDenormalize-region location)
				   (mapDenormalize-constellation location)
				   (mapDenormalize-id location)
				   timestamp))))
    (sql-sov-update-session-names
     (list (apply sovName (list sessionID
				scout
				timestamp))))
    (when (not (null? masterID))
      (sql-sov-update-session-slaves
       (list (apply sovSlave (list sessionID
				   masterID
				   timestamp))))))

  (send/back response-generator))

(define (exec-parse req)

  (define-values (sessionID region constellation system count)
    (values
     (extract-post-data req #"sessionID")
     (extract-post-data req #"region")
     (extract-post-data req #"constellation")
     (extract-post-data req #"system")
     (extract-post-data req #"count")))

  (define system-list
    (map (lambda (sys) (cons (vector-ref sys 0)
			     (vector-ref sys 1)))
	 (constellation->system-data (parse-constellation :id constellation))))

  (when (positive? (string->number count))
    (sql-sov-update-nodes
     (let ([timestamp (srfi-date->sql-timestamp (current-date))])
       (map (lambda (node)
	      (apply sovNode (list (first node)
				   (second node)
				   (third node)
				   (cdr (assoc (fourth node) system-list))
				   sessionID
				   "NULL"
				   timestamp)))
	    (node-data->list (combine-node-data req))))))

  (redirect-to (string-append "result" "/" sessionID)))

(define (exec-result-pre req [session ""])
  (if (not (false? (sql-sov-get-session session)))
      (exec-result req session)
      (response/full 404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE null null)))

(define (exec-result-post req [session ""])
  
  (map (lambda (x)
	 (let ([node (sql-parse->struct (sql-sov-get-node-data (binding-id x)) #:struct sovNode)])
	   (match (bytes->string/utf-8 (binding:form-value x))
	     ["delete" (when (not (false? node)) (set-sovNode-status! node "DELETED"))]
	     ["entosis" (when (not (false? node)) (set-sovNode-status! node "ENTOSIS"))]
	     [else (when (not (false? node)) (set-sovNode-status! node "NULL"))])
	   (sql-sov-update-nodes (list node))))
       (sort (request-bindings/raw req) bytes>? #:key binding:form-value))

  (exec-result req session))

(define (exec-result req [session ""])
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "Fuzzysov Node Reporting"
	  #:tablesorter #t
	  #:sort-column 3
	  (list (style/inline 'type: "text/css" ".form-description:after { content: ':'; margin-right: 0.75em; }")
		(style/inline 'type: "text/css" ".form-field { margin-right: 0.75em; }")
		(style/inline 'type: "text/css" ".form-entry { display: flex; flex-flow: row wrap; margin-bottom: 1em; }")))
	 (head (title "Fuzzysov Node Reporting")
	       (list
		(style/inline 'type: "text/css" ".data { max-width: 42em; }")))
	 (body
	  (h1 (format "~a » ~a » ~a" region constellation system))
	  (output:create-html-hint :tablesorter)
	  (let ([node-count (sql-sov-get-count sessionID)])
	    (h2 (format "Status: ~a sovereignty ~a" node-count (if (= node-count 1) "node" "nodes"))))
	  (form 'method: "POST" 'action: "/report"
		(input 'type: "hidden" 'name: "sessionID" 'value: sessionID 'readonly: #t)
		(input 'type: "hidden" 'name: "region" 'value: region 'readonly: #t)
		(input 'type: "hidden" 'name: "constellation" 'value: constellation 'readonly: #t)
		(input 'type: "hidden" 'name: "system" 'value: system 'readonly: #t)
		(input 'type: "hidden" 'name: "scout" 'value: scout 'readonly: #t)
		(div 'class: "form-entry"
		     (div 'class: "form-description" "Add new nodes")
		     (div 'class: "form-field"
			  (input 'type: "number" 'name: "count" 'min: 1 'max: 99 'value: 1))
		     (input 'type: "submit")))
	  (form 'method: "POST" 'target: "_self" 'id: "main" 'name: "main"
		(output:create-html-table #:head '("Name" "Type" "Target" "Datetime" "Status?" "")
					  #:drop-right 0
					  (sql-sov-get-nodes->table sessionID))
		(input 'type: "submit" 'id: "submit" 'value: "Update marked entries"))
	  (output:create-html-hint :updated)))
	out))))

  (define-values (sessionID region constellation system timestamp scout)
    (let ([poll (sql-sov-get-session->struct session)])
      (values
       (sovSession-sessionid poll)
       (sovSession-region poll)
       (sovSession-constellation poll)
       (sovSession-system poll)
       (sovSession-datetime poll)
       (sovName-name (sql-parse->struct (sql-sov-get-name session) #:struct sovName)))))

  (send/back response-generator))

;; Main

(define (main req)
  (main-dispatch req))

;; Servlet

(serve/servlet main
	       #:stateless? #t
	       #:port 8000
	       #:command-line? #t
	       #:banner? #t
	       #:servlet-regexp #rx""
	       ;; #:servlet-current-directory "/dev/shm/"
	       #:mime-types-path (build-path "/etc/mime.types"))
