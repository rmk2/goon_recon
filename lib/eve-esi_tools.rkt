#lang racket

(require json)
(require net/url)
(require file/gunzip)

(require "eve-list_tools.rkt")
(require "eve-api_tools.rkt")
(require "eve-sql_types.rkt")

(require (for-syntax racket/syntax))

(provide (all-defined-out))

;; ESI: macro to build proper ESI request URLs

(define-syntax (esi-build-url stx)
  (with-syntax ([schema "https"]
		[host "esi.tech.ccp.is"])
    (syntax-case stx ()
      [(_ path ... ((b . c) ...))
       #'(url->string
	  (make-url schema
		    #f
		    host
		    #f
		    #t
		    (map (lambda (x) (path/param (format "~a" x) null)) (list "latest" path ... ""))
		    (list (cons (string->symbol (format "~a" (syntax->datum #'b))) (format "~a" c)) ...)
		    #f))]
      [(_ path ... '((b . c) ...))
       #'(esi-build-url path ... ((b . c) ...))]
      [(_ path ... (_ (b . c) ...))
       #'(esi-build-url path ... ((b . c) ...))]
      [(_ path ...)
       #'(esi-build-url path ... (()))])))

;; ESI: request ID -> result, output: hash

(define-syntax (esi-hash-poll stx)
  (syntax-case stx ()
    [(_ type lst)
     (with-syntax ([make-type (format "~as" (syntax->datum #'type))]
		   [make-id (format-symbol "~a_id" (syntax->datum #'type))])
       #'(map (lambda (id) (hash-set (json-api (esi-build-url make-type id))
				     'make-id
				     id))
	      lst))]))

;; ESI: extract corp|alliance ID,Ticker,Name from result, output: list

(define-syntax (esi-hash-parse stx)
  (syntax-case stx ()
    [(_ type lst)
     (with-syntax ([make-id (format-symbol "~a_id" (syntax->datum #'type))]
		   [make-name (format-symbol "~a_name" (syntax->datum #'type))]
		   [make-raw (format "~a" (syntax->datum #'type))])
       #'(map (lambda (hash) (list (hash-ref hash 'make-id)
				   (hash-ref hash 'ticker)
				   (hash-ref hash 'make-name)))
	      lst))]))

;; ESI: extract corp|alliance ID,Ticker,Name from result, output: struct

(define-syntax (esi-hash-parse->struct stx)
  (syntax-case stx ()
    [(_ type lst)
     (with-syntax ([make-struct (format-id #'type "custom~as" (string-titlecase (syntax->datum #'type)))])
       #'(map (lambda (content) (apply make-struct content))
	      (esi-hash-parse type lst)))]))

;; ESI: recursive hash-ref to extract data from ESI's swagger definition
;; definition: https://esi.tech.ccp.is/latest/swagger.json

(define (esi-hash-ref hash lst)
  (let loop ([data hash] [refs lst] [i 0])
    (cond [(and (< i (length refs))
		(hash-has-key? data (list-ref refs i)))
	   (loop (hash-ref data (list-ref refs i)) refs (+ i 1))]
	  [else data])))

;; (define (esi-get-properties call)
;;   (hash-keys
;;    (esi-hash-ref swagger (list 'paths call 'get 'responses '|200| 'schema 'properties))))

;; Wrapper around esi-build-url for ESI's /search path

(define/contract (esi-search input lst)
  (-> string? (and/c (listof string?) (not/c empty?)) string?)
  (url->string
   (struct-copy url (string->url (esi-build-url "search"))
		[query (list (cons (string->symbol "search") input)
			     (cons (string->symbol "strict") "true")
			     (cons (string->symbol "categories")
				   (string-join lst ",")))])))

;; Replacement for fill-alliance from eve-dscan_tools.rkt
;; This consists of TWO queries to the API and ONE query to SQL
;; API = corp ticker -> API = alliance id -> SQL = alliance ticker

(define (esi-try-corporation input)
  ;; (match (json-api (esi-build-url "search" '((search . input) (categories . "corporation") (strict . "true"))))
  (match (json-api (esi-search input '("corporation")))
    [(hash-table ('corporation id)) (car id)]
    [else #f]))

(define (esi-corporation->alliance id)
  (match (json-api (esi-build-url "corporations" id))
    [(hash-table ('alliance_id id)) id]
    [else #f]))

(define (esi-fill-alliance input)
  (let* ([maybe-corp (esi-try-corporation input)]
	 [maybe-id (if maybe-corp (esi-corporation->alliance maybe-corp) #f)])
    (cond [(not (false? maybe-id))
	   (parse-alliance :ticker maybe-id)]
	  ;; [(and (string? input) (= (string-length input) 5))
	  ;;  input]
	  [else ""])))

;; Convenience bindings

(define (esi-hash-poll-corporation lst)
  (esi-hash-poll "corporation" lst))

(define (esi-hash-poll-alliance lst)
  (esi-hash-poll "alliance" lst))

(define (esi-hash-poll-character lst)
  (esi-hash-poll "character" lst))

(define (esi-hash-parse-corporation lst)
  (esi-hash-parse "corporation" lst))

(define (esi-hash-parse-alliance lst)
  (esi-hash-parse "alliance" lst))
