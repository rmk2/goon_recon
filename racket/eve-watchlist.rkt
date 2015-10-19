#! /usr/bin/env racket
#lang racket

(require json)
(require net/url)

(define coalition-tags (make-parameter #t))

;; Data fetching

(define (unify-data)
  (let ([collected-file "/var/www/servers/eve.rmk2.org/pages/eve-intel_retroactive.txt"]
	[regions-file "/var/www/servers/eve.rmk2.org/pages/eve-intel_regions.txt"])
    (if (and (file-exists? collected-file) (file-exists? regions-file))
	(append (file->lines collected-file) (file->lines regions-file))
	(let ([collected "http://eve.rmk2.org/eve-intel_retroactive.txt"]
	      [regions "http://eve.rmk2.org/eve-intel_regions.txt"])
	  (append (call/input-url (string->url collected) get-pure-port port->lines)
		  (call/input-url (string->url regions) get-pure-port port->lines))))))

(define (create-input)
  (map (lambda (l) (list (list-ref l 1)
			 (list-ref l 0)
			 (list-ref l 3)))
       (input-map-split (unify-data))))

(define coalitions
  (let [(file "/var/www/servers/eve.rmk2.org/pages/coalitions.txt")]
    (if (file-exists? file)
	(file->lines file)
	(call/input-url (string->url "http://eve.rmk2.org/coalitions.txt")
			get-pure-port
			port->lines))))

;; Data handling

(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

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

(define (hash-data)
  (map (lambda (l) (make-hash l)) (cons-data
				   (reverse
				    (remove-duplicates (reverse (create-input))
						       #:key (lambda (x) (car x))
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
		   [else "Other"])
		 str))))

;; Find out if a given alliance is part of a coalition

(define-syntax find-coalition
  (syntax-rules ()
    ((_ str) (if (coalition-tags)
		 (if (not (assoc str (pair-coalitions)))
		     str
		     (cdr (assoc str (pair-coalitions))))
		 str))))

;; Allow us to filter (and [kind of] pretty-print) watchlist data via regexp

(define-syntax filter-results
  (syntax-rules (:tags :alliance :hash :hash-tag)
    ((_ :tags filter) (begin (coalition-tags #t) (filter-results filter)))
    ((_ :alliance filter) (begin (coalition-tags #f) (filter-results filter)))
    ((_ filter) (for-each (lambda (hash)
			    (when (memf (lambda (x) (regexp-match filter x)) (hash-values hash))
			      (println (hash-values hash))))
			  (hash-data)))
    ((_ :hash filter) (filter-map (lambda (hash)
				    (if (memf (lambda (x) (regexp-match filter x)) (hash-values hash))
					hash
					#f))
				  (hash-data)))
    ((_ :hash-tag filter) (filter-map (lambda (x)
					(if (regexp-match filter (hash-ref x 'tag))
					    x
					    #f))
				      (hash-data)))))

;; Exec

;; (write-json (filter-results :hash "PL|DUMP|DRF|PROVI|STAIN|RUS|NC|Other"))

(write-json (filter-results :hash-tag "^(?!IMP).+"))
