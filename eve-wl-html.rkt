#! /usr/bin/env racket
#lang racket

(require net/url)
(require scribble/html)

(define (create-filenames)
  (let ([types '("all" "major" "minor")]
	[interval '("latest" "delayed")]
	[n '("" "-short")])
    (flatten (map (lambda (x)
		    (map (lambda (y)
			   (map (lambda (z) (string-append x "_" y z ".js")) n))
			 interval))
		  types))))

(define (create-links)
  (let ([snippet "javascript:(function(){document.body.appendChild(document.createElement('script')).src='https://eve.rmk2.org/URL';})()"])
    (map (lambda (js)
	   (tr
	    (td
	     (literal (a 'href: (string-replace snippet "URL" js)(string-replace js ".js" ""))))))
	 (create-filenames))))

(output-xml (doctype 'html))
(output-xml
 (html
  (head
   (title "Watchlist importer links")
   (literal (style/inline type: "text/css" "table { margin-bottom: 1em; }")))
  (body
   (h1 "Watchlist importer links")
   (p "These links work in Firefox, but might not work in Chrome/Chromium!")
   (let loop ([links (create-links)] [i 0] [result '()])
     (if (< i (length links))
	 (loop links [+ i 2] (append result (list (table (list-ref links i) (list-ref links (+ i 1))))))
	 result)))))
