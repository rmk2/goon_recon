#! /usr/bin/env racket
#lang racket

(require eve)
(require net/cgi)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

(define cgi-data (get-bindings))
(define filter_region (extract-bindings 'region cgi-data))

(define regions
  '("A-R00001" "A-R00002" "A-R00003" "Aridia" "B-R00004" "B-R00005" "B-R00006" "B-R00007" "B-R00008" "Black Rise" "Branch" "C-R00009" "C-R00010" "C-R00011" "C-R00012" "C-R00013" "C-R00014" "C-R00015" "Cache" "Catch" "Cloud Ring" "Cobalt Edge" "Curse" "D-R00016" "D-R00017" "D-R00018" "D-R00019" "D-R00020" "D-R00021" "D-R00022" "D-R00023" "Deklein" "Delve" "Derelik" "Detorid" "Devoid" "Domain" "E-R00024" "E-R00025" "E-R00026" "E-R00027" "E-R00028" "E-R00029" "Esoteria" "Essence" "Etherium Reach" "Everyshore" "F-R00030" "Fade" "Feythabolis" "Fountain" "G-R00031" "Geminate" "Genesis" "Great Wildlands" "H-R00032" "Heimatar" "Immensea" "Impass" "Insmother" "K-R00033" "Kador" "Khanid" "Kor-Azor" "Lonetrek" "Malpais" "Metropolis" "Molden Heath" "Oasa" "Omist" "Outer Passage" "Outer Ring" "Paragon Soul" "Period Basis" "Perrigen Falls" "Placid" "Providence" "Pure Blind" "Querious" "Scalding Pass" "Sinq Laison" "Solitude" "Stain" "Syndicate" "Tash-Murkon" "Tenal" "Tenerifis" "The Bleak Lands" "The Citadel" "The Forge" "The Kalevala Expanse" "The Spire" "Tribute" "Vale of the Silent" "Venal" "Verge Vendor" "Wicked Creek"))

(define (region? query) (findf (lambda (str) (equal? (string-downcase query) (string-downcase str))) regions))

(define query-regions (cond [(empty? filter_region) null]
			    [(and (not (empty? filter_region)) (string-empty? (car filter_region))) null]
			    [else (filter-map region? (string-split (car filter_region) ","))]))

(define (sql-moon-region-towers param)
  (map vector->list (query-rows sqlc "SELECT * FROM moonScanView WHERE regionName LIKE ?" param)))

(define (sql-moon-get-towers)
  (query-rows sqlc "SELECT * FROM moonScanView"))

(output-http-headers)
(output-xml (doctype 'html))
(output-xml
 (html
  (output:create-html-head #:title "Moon Scan Data"
			   #:tablesorter #t
			   #:sort-column 0
			   (list (literal (style/inline 'type: "text/css" "tr > td[class=\"LOLTX\"], tr > td[class=\"OHGOD\"] { background-color: #4D6EFF; color: white; }"))
				 (literal (style/inline 'type: "text/css" "#bar { padding: 0.5em; float: right; }"))
				 (literal (style/inline 'type: "text/css" "td { white-space: normal; }"))))
  (body
   (div 'id: "bar"
	(form 'name: "filter" 'method: "get"
	      (label 'for: "region" "Select Region: ")
	      (select 'name: "region"
		      (optgroup 'label: "Show all regions" (option 'value: "" "Any"))
		      (optgroup 'label: "K-Space"
				(map option (filter (lambda (x) (regexp-match "^(?![A-Z0-9]-)" x)) regions)))
		      (optgroup 'label: "W-Space"
				(map option (filter (lambda (x) (regexp-match "^[A-Z0-9]-" x)) regions))))
	      nbsp
	      (input 'type: "submit")))
   (div 'id: "content"
	(h1 "Moon Scan Data")
	(output:create-html-hint (format "Results filtered for: Region (~a)" (string-join query-regions "|")))
	(output:create-html-hint :tablesorter)
	(output:create-html-table (cond
				   [(not (empty? query-regions))
				    (append-map (lambda (region) (sql-moon-region-towers region)) query-regions)]
				   [else (map vector->list (sql-moon-get-towers))])
				  #:ticker->class #t
				  #:head (list "Region"
					       "Constellation"
					       "System"
					       "Planet"
					       "Moon"
					       "CT"
					       "Alliance"
					       "AT"
					       "Corporation"
					       "Date"
					       "Tower"
					       "Goo"))
	(output:create-html-hint :updated)))))
