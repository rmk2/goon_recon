#lang racket

(require db)
(require racket/set)

(require "eve-sql_main.rkt")
(require "eve-string_tools.rkt")

(provide (all-defined-out))

;; Split a csv list

(define/contract (split-user-input lst)
  (-> list? list?)
  (cond [(empty? lst) null]
	[(and (not (empty? lst)) (string-empty? (car lst))) null]
	[else (string-split (car lst) ",")]))

(define/contract (map-split-user-input lst)
  (-> list? list?)
  (append-map split-user-input lst))

;; Macros

(define-syntax (sql-build-query stx)
  (syntax-case stx (: -> = != FROM WHERE IS NOT)
    ;; Syntactic sugar (SQL-like)
    [(_ FROM table) #'(sql-build-query table)]
    [(_ columns FROM table) #'(sql-build-query columns : table)]
    [(_ table WHERE query IS param) #'(sql-build-query table -> query = param)]
    [(_ FROM table WHERE query IS param) #'(sql-build-query table -> query = param)]
    [(_ table WHERE query IS NOT param) #'(sql-build-query table -> query != param)]
    [(_ FROM table WHERE query IS NOT param) #'(sql-build-query table -> query != param)]
    [(_ columns FROM table WHERE query IS param) #'(sql-build-query table -> query = param)]
    [(_ columns FROM table WHERE query IS NOT param) #'(sql-build-query table -> query != param)]
    ;; Main functions
    [(_ table)
     #'(query-rows sqlc (format "SELECT * FROM ~a" table))]
    [(_ columns : table)
     #'(query-rows sqlc (format "SELECT ~a FROM ~a" columns table))]
    [(_ table -> query = param)
     #'(query-rows sqlc (format "SELECT * FROM ~a WHERE ~a = ?" table query) param)]
    [(_ table -> query != param)
     #'(query-rows sqlc (format "SELECT * FROM ~a WHERE ~a != ?" table query) param)]
    [(_ columns : table -> query = param)
     #'(query-rows sqlc (format "SELECT ~a FROM ~a WHERE ~a = ?" columns table query) param)]
    [(_ columns : table -> query != param)
     #'(query-rows sqlc (format "SELECT ~a FROM ~a WHERE ~a != ?" columns table query) param)]))

(define-syntax (sql-build-test stx)
  (syntax-case stx (:known-good :direct)
    [(_ id :known-good column table)
     ;; Check query against a list of known-good candidates from database
     #'(define/contract (id query)
	 (-> string? any)
	 (findf (lambda (str) (equal? (string-downcase query) (string-downcase str)))
		(map vector->values
		     (query-rows sqlc (format "SELECT DISTINCT ~a FROM ~a" column table)))))]
    [(_ id :direct column table)
     ;; Check query directly against database
     #'(define/contract (id query)
	 (-> string? any)
	 (if (false? (query-maybe-value sqlc (format "SELECT ~a FROM ~a WHERE ~a = ?" column table column) query))
	     #f
	     query))]
    [(_ id column table) #'(sql-build-test id :known-good column table)]))

;; Define filter conditionals

(sql-build-test system? :known-good "solarSystemName" "mapSolarSystems")
(sql-build-test constellation? :known-good "constellationName" "mapConstellations")
(sql-build-test region? :known-good "regionName" "mapRegions")
(sql-build-test alliance? :known-good "allianceName" "customAlliances")
(sql-build-test alliance-ticker? :known-good "allianceTicker" "customAlliances")
(sql-build-test corporation? :direct "corporationName" "customCorporations")
(sql-build-test corporation-ticker? :direct "corporationTicker" "customCorporations")

;; Query "columns" in SQL "table" for a list/string

(define (sql-get-by-filter lst #:table table #:columns [columns "*"] #:union? [union? #t])
  (define/contract (lookup str)
    (-> string? any)
    (cond
     [(region? str) (sql-build-query columns : table -> "regionName" = str)]
     [(constellation? str) (sql-build-query columns : table -> "constellationName" = str)]
     [(system? str) (sql-build-query columns : table -> "solarSystemName" = str)]
     [(alliance? str) (sql-build-query columns : table -> "allianceName" = str)]
     [(corporation? str) (sql-build-query columns : table -> "corporationName" = str)]
     [(alliance-ticker? str) (sql-build-query columns : table -> "allianceTicker" = str)]
     [(corporation-ticker? str) (sql-build-query columns : table -> "corporationTicker" = str)]
     [else null]))
  (let* ([origin lst]
	 [result
	  (match origin
	    ;; [(? string? a (and a (pregexp "^([-+_.A-Za-z0-9 ]+[,]{1})+")))
	    ;;  (map lookup (remove-duplicates (split-input a)))]
	    ;; [(? string? a) (lookup a)]
	    [(list a) (lookup a)]
	    [(list a ...) (map lookup (remove-duplicates a))])])
    (map vector->list
	 (cond [(and union? (> (length origin) 1))
	       	(apply set-union result)]
	       [(and (false? union?) (> (length origin) 1))
	       	(apply set-intersect result)]
	       [else result]))))