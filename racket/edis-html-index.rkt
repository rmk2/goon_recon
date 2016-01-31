#! /usr/bin/env racket
#lang racket

(require scribble/html)

(define prefix (make-parameter "/var/www/servers/"))
(define suffix (make-parameter "/pages/"))
(define index-file (make-parameter "index.html"))

(define domains (make-parameter null))

(define parse-args
  (command-line
   #:multi
   [("-d" "--domain") str "Select a domain for which to write an index.html" (domains (cons str (domains)))]
   #:once-each
   [("-p" "--prefix") str "Prefix, default: /var/www/servers/" (prefix str)]
   [("-s" "--suffix") str "Suffix, default: /pages/" (suffix str)]
   [("-o" "--output" "-f" "--file") str "Name of index file, default: index.html" (index-file str)]))

(define (output-html name)
  (output-xml (doctype 'html))
  (output-xml
   (html
    (head
     (title name)
     (literal (style/inline type: "text/css" "h1 { text-align: center; margin: 10% 0 auto; font-size: xx-large; }"))
     (literal (style/inline type: "text/css" ".content { text-align: center; margin: 1em; }")))
    (body
     (h1 name)
     (div 'class: "content"
	  (p "Move along, nothing to see here!"))))))

(define (main)
  (for-each (lambda (name)
	 (with-output-to-file (string-append (prefix) name (suffix) (index-file))
	   (lambda () (output-html name))
	   #:exists 'truncate/replace))
       (domains)))

(main)
