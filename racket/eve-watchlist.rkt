#! /usr/bin/env racket
#lang racket

(require eve)

(define coalition-tags (make-parameter #t))
(define query-raw (make-parameter #f))

;; Command-line argument handlung

(define cl-filter (make-parameter "^(?!IMP).+")) ;; Default: show hostile only
(define cl-length (make-parameter 1000)) ;; Default list length: 1000

(define (check-input input)
  (when (string? input)
    (cl-filter (pregexp (string-append "^(?i:" (string-replace input " " "|") ").*")))))

(define parse-args
  (command-line
   #:once-each
   [("-T" "--no-tags") "Apply filter to whole data, not just coalition tags" (coalition-tags #f)]
   [("-l" "-n" "--length") n "Watchlist length" (if (< (string->number n) 1024)
						    (cl-length (string->number n))
						    (exit (println "List length cannot be greater than 1024")))]
   [("-r" "--raw") "Use raw super data (for backward compatibility); default: off" (query-raw #t)]
   #:once-any
   [("-B" "--blue" "--friendly") "Only output friendly entities" (cl-filter "^(?=IMP).+")]
   [("-R" "--red" "--hostile") "Only output hostile entities" (cl-filter)]
   [("-c" "--custom" "-e" "--regexp") str "Create a custom filter" (check-input str)]))

;; Data fetching

(define (create-input)
  (map (lambda (v) (list (vector-ref v 0)
			 (vector-ref v 1)
			 (vector-ref v 2)))
       (sql-filter-watchlist)))

(define coalitions
  (let [(file "/var/www/servers/eve.rmk2.org/pages/coalitions.txt")]
    (if (file-exists? file)
	(file->lines file)
	(call/input-url (string->url "https://eve.rmk2.org/coalitions.txt")
			get-pure-port
			port->lines))))

;; Data handling

(define (cons-data list)
  (map (lambda (i)
	 (let loop ([item i] [i 0] [result '()])
	   (cond
	    [(and (zero? i) (< i (length item))) (loop item (+ i 1) (list* (cons 'name (list-ref item i)) result))]
	    [(and (= i 1) (< i (length item))) (loop item (+ i 1) (list* (cons 'ship (list-ref item i)) result))]
	    [(and (= i 2) (< i (length item)))
	     (loop item (+ i 1) (list* (cons 'tag (rewrite-tag (find-coalition (list-ref item i)))) result))]
	    [(and (= i 2) (>= i (length item)))
	     (loop item (+ i 1) (list* (cons 'tag (rewrite-tag (find-coalition "None"))) result))]
	    [else result])))
       list))

(define (hash-data lst)
  (map (lambda (l) (make-hash l))
       (cons-data
	(reverse
	 (remove-duplicates (reverse lst)
			    #:key (lambda (x) (string-downcase (car x)))
			    string=?)))))

;; Create an assoc-ready list of pairs for coalition data: (alliance . coalition)

(define (pair-coalitions)
  (map (lambda (x) (cons (cadr x) (car x))) (input-map-split coalitions)))

;; Change coalition names to shorthand tags

(define-syntax rewrite-tag
  (syntax-rules ()
    ((_ str) (if (coalition-tags)
		 (case str
		   [("Pandemic Legion") "PL"]
		   [("The Imperium") "IMP"]
		   [("Guardians of the Galaxy") "DUMP"]
		   [("Drone Region Federation") "DRF"]
		   [("Provi-Bloc") "PROVI"]
		   [("Stain Wagon") "STAIN"]
		   [("Red Menace") "RUS"]
		   [("Northern Coalition.") "NC"]
		   [("The-Culture") "TC"]
		   [("Dead Terrorists"
		     "Snuffed Out"
		     "Shadow Cartel"
		     "Psychotic Tendencies."
		     "Cynosural Field Theory."
		     "LowSechnaya Sholupen"
		     "Project.Mayhem.") "Lowsec"]
		   [else "Other"])
		 str))))

;; Find out if a given alliance is part of a coalition

(define-syntax find-coalition
  (syntax-rules ()
    ((_ str) (if (coalition-tags)
		 (if (assoc str (pair-coalitions))
		     (cdr (assoc str (pair-coalitions)))
		     str)
		 str))))

;; Allow us to filter (and [kind of] pretty-print) watchlist data via regexp

(define-syntax filter-results
  (syntax-rules (:tags :alliance :hash :hash-tag)
    ((_ :tags filter) (begin (coalition-tags #t) (filter-results filter)))
    ((_ :alliance filter) (begin (coalition-tags #f) (filter-results filter)))
    ((_ filter) (for-each (lambda (hash)
			    (when (memf (lambda (x) (regexp-match filter x)) (hash-values hash))
			      (println (hash-values hash))))
			  (hash-data (create-input))))
    ((_ :hash filter) (filter-map (lambda (hash)
				    (if (memf (lambda (x) (regexp-match filter x)) (hash-values hash))
					hash
					#f))
				  (hash-data (create-input))))
    ((_ :hash-tag filter) (filter-map (lambda (x)
					(if (regexp-match filter (hash-ref x 'tag))
					    x
					    #f))
				      (hash-data (create-input))))))

;; Curtail list to a maximum of n unique entries (default: n = (cl-length) = 1000)

(define-syntax curtail-list
  (syntax-rules (:length)
    ((_ :length n list) (if (> (length list) n)
			    (list-tail list (- (length list) n))
			    list))
    ((_ list) (curtail-list :length (cl-length) list))))

;; Exec

;; (write-json (filter-results :hash "PL|DUMP|DRF|PROVI|STAIN|RUS|NC|Other"))

;; (write-json (curtail-list (filter-results :hash-tag (cl-filter))))

(write-json (curtail-list (if (coalition-tags)
			      (filter-results :hash-tag (cl-filter))
			      (filter-results :hash (cl-filter)))))
