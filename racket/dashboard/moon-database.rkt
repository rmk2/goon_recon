#! /usr/bin/env racket
#lang racket

(require eve)
(require net/cgi)
(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

(define (sql-moon-get-towers)
  (query-rows sqlc "SELECT * FROM moonScanView"))

(output-http-headers)
(output-xml (doctype 'html))
(output-xml
 (html
  (output:create-html-head #:title "Moon Scan Data" #:sort-column 0)
  (body
   (div 'id: "content"
	(h1 "Moon Scan Data")
	(output:create-html-hint :tablesorter)
	(output:create-html-table (map vector->list (sql-moon-get-towers))
				  #:head (list "Region"
					       "Constellation"
					       "System"
					       "Planet"
					       "Moon"
					       "Ticker"
					       "Alliance"
					       "Ticker"
					       "Corporation"
					       "Date"
					       "Tower"
					       "Goo"))
	(output:create-html-hint :updated)))))
