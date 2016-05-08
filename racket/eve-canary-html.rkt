#! /usr/bin/env racket
#lang racket

(require eve)

(require scribble/html/html)
(require (only-in scribble/html/xml
		  literal
		  output-xml))

(output-xml (doctype 'html))
(output-xml
 (html
  (output:create-html-head #:title "Alliance Canary Watchlist")
  (body
   (div 'id: "content"
	(h1 "Alliance Canary Watchlist")
	(output:create-html-hint "Criteria: last event = kill, last event <= 60 days, activity >= ⟨alliance⟩+1σ")
	(output:create-html-hint :tablesorter)
	(output:create-html-table (map vector->list (sql-canary-get-watchlist))
				  #:head (list "Shiptype"
					       "Name"
					       "Corporation"
					       "Alliance"
					       "System"
					       "Region"
					       "Date"
					       "Activity (x̅)"
					       "±Hours (σ)"))
	(output:create-html-hint :updated)))))
