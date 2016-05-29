#! /usr/bin/env racket
#lang racket

(require web-server/templates)
(require json)

(define prefix (make-parameter "./"))
(define config (make-parameter "setup.json"))

(define parse-args
  (command-line
   #:once-each
   [("-p" "--prefix") dir "Choose output folder"
    (if (directory-exists? dir)
	(prefix dir)
	(begin (make-directory dir) (prefix dir)))]
   [("-c" "--config") file "Choose a configuration file"
    (if (file-exists? file)
	(config file)
	(error "Configuration file does not exist"))]))

(define (create-setup-list)
  (map (lambda (x) (make-hash x))
       (list
	'((type . "all") (length . 1000) (id . "delayed"))
	'((type . "all") (length . 750) (id . "delayed"))
	'((type . "major") (length . 1000) (id . "delayed"))
	'((type . "major") (length . 750) (id . "delayed"))
	'((type . "minor") (length . 1000) (id . "delayed"))
	'((type . "minor") (length . 750) (id . "delayed"))
	'((type . "all") (length . 1000) (id . "latest"))
	'((type . "all") (length . 750) (id . "latest"))
	'((type . "major") (length . 1000) (id . "latest"))
	'((type . "major") (length . 750) (id . "latest"))
	'((type . "minor") (length . 1000) (id . "latest"))
	'((type . "minor") (length . 750) (id . "latest")))))

(define (template-display [type "all"] [id "lastest"] [length 1000] [prefix ""])
  (let* ([type type]
	 [length length]
	 [name (string-append type "_" id)]
	 [filename (string-append name (cond [(= length 750) "-short"]
					     [(= length 1000) ""]
					     [else (string-append "-" (number->string length))]) ".js")]
	 [template (string-append name "-" (number->string length) ".js")])
    (display-to-file (include-template "./watchlist.template")
		     (string-append prefix "/" filename)
		     #:exists 'truncate/replace)))

(define setup-list (let ([file (config)])
		     (if (file-exists? file)
			 (read-json (open-input-file file))
			 (create-setup-list))))

(define (main)
  (for-each (lambda (hash) (template-display (hash-ref hash 'type)
					     (hash-ref hash 'id)
					     (hash-ref hash 'length)
					     (prefix)))
	    setup-list))

(main)
