#lang racket

(require json)
(require xml/path)
(require net/url)
(require file/gunzip)

(provide (all-defined-out))

;; Generic CREST (json) API polling function; output: jsexpr

(define-syntax json-api
  (syntax-rules (:gzip :plain)
    ((_ :plain str)
     (bytes->jsexpr
      (call/input-url (string->url str)
		      get-pure-port
		      port->bytes
		      '("User-Agent: ryko@rmk2.org"))))
    ((_ :gzip str)
     (bytes->jsexpr
      (call/input-url (string->url str)
		      get-pure-port
		      (lambda (input) (call-with-output-bytes (lambda (x) (gunzip-through-ports input x))))
		      '("Accept-Encoding: gzip" "User-Agent: ryko@rmk2.org"))))
    ((_ str) (json-api :gzip str))))

;; Generic APIv2 (xml) API polling function; output: string

(define-syntax xml-api
  (syntax-rules (:gzip :plain)
    ((_ :plain str)
     (call/input-url (string->url str)
		     get-pure-port
		     port->string
		     '("User-Agent: ryko@rmk2.org")))
    ((_ :gzip str)
     (call/input-url (string->url str)
		     get-pure-port
		     (lambda (input) (call-with-output-string (lambda (x) (gunzip-through-ports input x))))
		     '("Accept-Encoding: gzip" "User-Agent: ryko@rmk2.org")))
    ((_ str) (xml-api :plain str))))

;; Extract XML APIv2 response bodies

(define (rowset->hash lst)
  (filter-map (lambda (x) (if (list? x)
			      (make-hash (map (lambda (y) (cons (car y) (cadr y))) (cadr x)))
			      #f))
	      (se-path*/list '(rowset) lst)))

;; Convert specified hash content into csv data

(define-syntax input-hash-join 
  (syntax-rules ()
    ((_ hash key) (string-join (map (lambda (x) (hash-ref x key)) hash) ","))))
