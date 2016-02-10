#lang racket

(require net/url)
(require "eve-list_tools.rkt")

(provide (all-defined-out))

;; Poll unified super intel from local file or via http(s)

(define (edis-data)
  (let ([collected-file "/var/www/servers/eve.rmk2.org/pages/eve-intel_retroactive.txt"]
	[regions-file "/var/www/servers/eve.rmk2.org/pages/eve-intel_regions.txt"])
    (if (and (file-exists? collected-file) (file-exists? regions-file))
	(append (file->lines collected-file) (file->lines regions-file))
	(let ([collected "https://eve.rmk2.org/eve-intel_retroactive.txt"]
	      [regions "https://eve.rmk2.org/eve-intel_regions.txt"])
	  (append (call/input-url (string->url collected) get-pure-port port->lines)
		  (call/input-url (string->url regions) get-pure-port port->lines))))))

;; Solarsystem data parsing

(define solar-list
  (make-hash
   (input-map-split
    (file->lines "/home/ryko/eve-solarsystemids"))))

(define-syntax solar-parse
  (syntax-rules (:system :region)
    ((_ :system str) (car (hash-ref solar-list str)))
    ((_ :region str) (cadr (hash-ref solar-list str)))
    ((_ str) (string-join (hash-ref solar-list str) ","))))
