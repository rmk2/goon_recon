#! /usr/bin/env racket
#lang racket

(require eve)

(output-xml (doctype 'html))
(output-xml
 (html
  (output:create-html-head #:title "Alliance Canary Watchlist" #:sort-column 3)
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
