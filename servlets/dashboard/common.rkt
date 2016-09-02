#lang racket

(require eve)

(require (for-syntax racket/syntax))

(provide (all-defined-out))

;; Define maximum admissable distance (in km) for moons/towers

(define max_distance (make-parameter 10000))

(define (sql-get-scanned-regions table)
  (map vector->values (query-rows sqlc (string-append "SELECT DISTINCT regionName FROM " table " ORDER BY regionName"))))

(define (query-regions lst) (cond [(empty? lst) null]
				  [(and (not (empty? lst)) (string-empty? (car lst))) null]
				  [else (filter-map region? (string-split (car lst) ","))]))

(define (user-filter-regions lst #:filter-function filter-function #:function function)
  (cond
   [(not (empty? (query-regions lst)))
    (append-map (lambda (region) (filter-function region))
		(query-regions lst))]
   [else function]))

(define (get-filter req filter)
  (match
    (bindings-assq
     filter
     (request-bindings/raw req))
    [(? binding:form? b)
     (list
      (bytes->string/utf-8
       (binding:form-value b)))]
    [_ null]))

(define (extract-post-data req query)
  (match
    (bindings-assq
     query
     (request-bindings/raw req))
    [(? binding:form? b)
     (bytes->string/utf-8
      (binding:form-value b))]
    [_ null]))

;; Define filter-* from URL parameters via (get-filter), check their validity

(define-syntax (sql-bind-user-input stx)
  (syntax-case stx ()
    [(_ type #:request req)
     (with-syntax ([make-id (format-id #'type "filter-~a" (syntax->datum #'type))]
		   [make-test (format-id #'type "~a?" (syntax->datum #'type))]
		   [make-bind (string->bytes/utf-8 (format "~a" (syntax->datum #'type)))])
       #'(define make-id
	   (filter-map make-test (split-user-input (get-filter req make-bind)))))]))

;; Print a list of applied filters

(define-syntax print-filters
  (syntax-rules ()
    ((_ a b c)
     (format "Results filtered for: Region (~a), Constellation (~a), System (~a)"
	     (string-join a "|")
	     (string-join b "|")
	     (string-join c "|")))
    ((_ a b c d)
     (format "~a , Alliance (~a)"
	     (print-filters a b c)
	     (string-join (map (lambda (x) (parse-alliance :ticker x)) d) "|")))
    ((_ a b c d e)
     (format "~a, Corporation (~a)"
	     (print-filters a b c d)
	     (string-join (map (lambda (x) (parse-corporation :ticker x)) e) "|")))))
