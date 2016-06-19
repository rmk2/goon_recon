#! /usr/bin/env racket
#lang racket

(require eve)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

;; Read from stdin

(define pipe-input (let ([input (second (read))])
		     (cond [(empty? input) (exit 0)]
			   [else (filter sql-killmail? (car input))])))

;; Only show us (known) money moons, cons their type to the killmail data

(define (query-money-moons input-list)
  (filter-map (lambda (x)
		(let ([moon (parse-moon :name (sql-killmail-location x))])
		  (if (money-moon? moon)
		      (cons x (last (vector->list (parse-moondata (split-moon-display moon)))))
		      #f)))
	      input-list))

;; Filter uniques, keep the newer result

(define (filter-unique lst)
  (reverse
   (remove-duplicates (reverse lst)
		      #:key (lambda (x) (sql-killmail-location (car x))))))

;; Parse typeIDs

(define (parse-money-moons input-list)
  (map (lambda (goo-list)
	 (let ([km (car goo-list)] [goo (cdr goo-list)])
	   (if (positive? (sql-killmail-location km))
	       (let ([location (sql-parse->struct (parse-map (sql-killmail-location km))
						  #:struct mapDenormalize)])
		 (list
		  (parse-type :name (sql-killmail-shiptype km))
		  (sql-killmail-corporationname km)
		  (sql-killmail-alliancename km)
		  (simplify-moon-display (mapDenormalize-name location))
		  (parse-region :name (sql-killmail-region km))
		  (sql-killmail-datetime km)
		  goo
		  (string-append "https://zkillboard.com/kill/" (number->string (sql-killmail-killid km)))))
	       #f)))
       input-list))

;; HTML Output

(define (output-html lst)
  (begin
    (output-xml (doctype 'html))
    (output-xml
     (html
      (output:create-html-head #:title "EVE Money Moon Digest" #:sort-column 5)
      (body
       (div 'id: "content"
	    (h1 "EVE Money Moon Digest")
	    (output:create-html-hint :tablesorter)
	    (h2 "Losses")
	    (output:create-html-table lst
				      #:head '("Type" "Corporation" "Alliance" "System" "Region" "Date" "Goo" "Link"))
	    (output:create-html-hint :updated)))))))

;; Exec

(output-html (parse-money-moons (filter-unique (query-money-moons pipe-input))))
