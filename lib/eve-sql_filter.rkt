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

;; Check whether SQL table contains a given column

(define/contract (column? table str)
  (-> string? string? (or/c string? #f))
  (query-maybe-value sqlc (string-append "SELECT COLUMN_NAME "
					 "FROM information_schema.COLUMNS "
					 "WHERE TABLE_SCHEMA = 'eve_sde' "
					 "AND TABLE_NAME = ? "
					 "AND COLUMN_NAME = ?")
		     table
		     str))

;; Macros

(define-syntax (sql-build-query stx)
  (syntax-case stx (: -> = != FROM WHERE IS NOT)
    ;; Syntactic sugar (SQL-like)
    [(_ FROM table) #'(sql-build-query table)]
    [(_ columns FROM table) #'(sql-build-query columns : table)]
    [(_ table WHERE query IS param) #'(sql-build-query table -> query = param)]
    [(_ FROM table WHERE query IS param) #'(sql-build-query table -> query = param)]
    [(_ table WHERE query OR alt IS param) #'(sql-build-query table -> query + alt = param)]
    [(_ FROM table WHERE query OR alt IS param) #'(sql-build-query table -> query + alt = param)]
    [(_ table WHERE query IS NOT param) #'(sql-build-query table -> query != param)]
    [(_ FROM table WHERE query IS NOT param) #'(sql-build-query table -> query != param)]
    [(_ columns FROM table WHERE query IS param) #'(sql-build-query columns : table -> query = param)]
    [(_ columns FROM table WHERE query OR alt IS param) #'(sql-build-query columns : table -> query + alt = param)]
    [(_ columns FROM table WHERE query IS NOT param) #'(sql-build-query columns : table -> query != param)]
    ;; Main functions
    [(_ table)
     #'(query-rows sqlc (format "SELECT * FROM ~a" table))]
    [(_ columns : table)
     #'(query-rows sqlc (format "SELECT ~a FROM ~a" columns table))]
    [(_ table -> query = param)
     #'(query-rows sqlc (format "SELECT * FROM ~a WHERE ~a = ?" table query) param)]
    [(_ table -> query + alt = param)
     #'(query-rows sqlc (format "SELECT * FROM ~a WHERE ~a = ? OR ~a = ?" table query alt) param param)]
    [(_ table -> query != param)
     #'(query-rows sqlc (format "SELECT * FROM ~a WHERE ~a != ?" table query) param)]
    [(_ columns : table -> query = param)
     #'(query-rows sqlc (format "SELECT ~a FROM ~a WHERE ~a = ?" columns table query) param)]
    [(_ columns : table -> query + alt = param)
     #'(query-rows sqlc (format "SELECT ~a FROM ~a WHERE ~a = ? OR ~a = ?" columns table query alt) param param)]
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
    [(_ id :known-good column table restriction-column restriction-param)
     ;; Check query against a list of known-good candidates from database
     #'(define/contract (id query)
	 (-> string? any)
	 (findf (lambda (str) (equal? (string-downcase query) (string-downcase str)))
		(map vector->values
		     (query-rows
		      sqlc
		      (format "SELECT DISTINCT ~a FROM ~a WHERE ~a = ?" column table restriction-column)
		      restriction-param))))]
    [(_ id :direct column table)
     ;; Check query directly against database
     #'(define/contract (id query)
	 (-> string? any)
	 (if (false? (query-maybe-value sqlc (format "SELECT ~a FROM ~a WHERE ~a = ?" column table column) query))
	     #f
	     query))]
    [(_ id :direct column1 column2 table)
     ;; Check query directly against database, column1 OR column2
     #'(define/contract (id query)
	 (-> string? any)
	 (if (false? (query-maybe-value
		      sqlc
		      (format "SELECT ~a FROM ~a WHERE ~a = ? OR ~a = ?" column1 table column1 column2)
		      query query))
	     #f
	     query))]
    [(_ id column table) #'(sql-build-test id :known-good column table)]))

;; Define filter conditionals

(sql-build-test system? :known-good "solarSystemName" "mapSolarSystems")
(sql-build-test constellation? :known-good "constellationName" "mapConstellations")
(sql-build-test region? :known-good "regionName" "mapRegions")
(sql-build-test goo? :known-good "typeName" "invTypes" "groupID" "427")
(sql-build-test structure? :known-good "structureType" "sovTimerboardView")
(sql-build-test type? :known-good "typeName" "invTypes")
(sql-build-test alliance? :direct "allianceName" "allianceTicker" "customAlliances")
(sql-build-test corporation? :direct "corporationName" "corporationTicker" "customCorporations")

;; Query "columns" in SQL "table" for a list/string

(define (sql-get-by-filter lst #:table table #:columns [columns "*"] #:union? [union? #t] #:vector? [vector->list? #t])
  (define/contract (lookup str)
    (-> string? any)
    (cond
     [(region? str) (sql-build-query columns : table -> "regionName" = str)]
     [(constellation? str) (sql-build-query columns : table -> "constellationName" = str)]
     [(system? str) (sql-build-query columns : table -> "solarSystemName" = str)]
     [(goo? str) (sql-build-query columns : table -> "moonType" = str)]
     [(structure? str) (sql-build-query columns : table -> "structureType" = str)]
     [(alliance? str) (sql-build-query columns : table -> "allianceName" + "allianceTicker" = str)]
     [(corporation? str) (sql-build-query columns : table -> "corporationName" + "corporationTicker" = str)]
     [(type? str) (sql-build-query columns : table -> "typeName" = str)]
     [else null]))
  (let* ([origin lst]
	 [result
	  (match origin
	    [(list (or (? string? a) (list (? string? a)))) (lookup a)]
	    [(list (or (? string? a) (list (? string? a))) ...) (map lookup (remove-duplicates a))]
	    [(list (list (? string? a) ...)) (append-map lookup (remove-duplicates a))]
	    [(list (or (list (? string? a) ...) (? string? a)) ...)
	     (map (lambda (x) (sql-get-by-filter x #:table table #:columns columns #:union? #t #:vector? #f)) a)])])
    (map (lambda (arg) (if vector->list? (vector->list arg) arg))
	 (cond [(and union? (> (length origin) 1))
	       	(apply set-union result)]
	       [(and (false? union?) (> (length origin) 1))
	       	(apply set-intersect result)]
	       [else result]))))
