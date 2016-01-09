#lang racket

(require json
	 net/url
	 file/gunzip
	 xml
	 xml/path
	 srfi/19)

(provide (all-from-out json
		       xml)
	 (all-defined-out))

;; Extract XML APIv2 response bodies

(define (rowset->hash lst)
  (filter-map (lambda (x) (if (list? x)
			      (make-hash (map (lambda (y) (cons (car y) (cadr y))) (cadr x)))
			      #f))
	      (se-path*/list '(rowset) lst)))

;; Split a list into members of length n

(define (split-list lst [n 100])
  (let loop ([query lst] [limit n] [i 1] [result null])
    (if (<= (* i limit) (length query))
	(loop query limit (+ i 1) (list* (drop (take query (* i limit)) (* (- i 1) limit)) result))
	(reverse (list* (take-right query (remainder (length query) limit)) result)))))

;; Convert specified hash content into csv data

(define-syntax input-hash-join 
  (syntax-rules ()
    ((_ hash key) (string-join (map (lambda (x) (hash-ref x key)) hash) ","))))

;; String-split/join each list-of-strings within in a given list

(define-syntax input-map-split
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-split x ",")) input))))

(define-syntax input-map-join
  (syntax-rules ()
    ((_ input) (map (lambda (x) (string-join x ",")) input))))

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

;; Generic CREST (json) API polling function; output: jsexpr

(define (json-api str)
  (bytes->jsexpr
   (call/input-url (string->url str)
		   get-pure-port
		   (lambda (input) (call-with-output-bytes (lambda (x) (gunzip-through-ports input x))))
		   '("Accept-Encoding: gzip" "User-Agent: ryko@rmk2.org"))))

;; Generic APIv2 (xml) API polling function; output: string

(define (xml-api str)
  (call/input-url (string->url str)
		  get-pure-port
		  (lambda (input) (call-with-output-string (lambda (x) (gunzip-through-ports input x))))
		  '("Accept-Encoding: gzip" "User-Agent: ryko@rmk2.org")))


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
