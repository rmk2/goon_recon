#lang racket

(require db)
(require db/util/datetime)
(require srfi/19)

(require (for-syntax racket/syntax)
	 (for-syntax syntax/parse))

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")

(provide (all-defined-out))

;; SQL

;; Create tables

(define (sql-auth-create-whitelist-corporations)
  (if (table-exists? sqlc "authWhitelistCorporations")
      #t
      (query-exec sqlc "CREATE TABLE authWhitelistCorporations ( corporationID INT, corporationTicker VARCHAR(5), corporationName VARCHAR(255), datetime DATETIME, user VARCHAR(255), PRIMARY KEY ( corporationID ), KEY ( corporationTicker ), KEY ( corporationName ) )")))

(define (sql-auth-create-whitelist-alliances)
  (if (table-exists? sqlc "authWhitelistAlliances")
      #t
      (query-exec sqlc "CREATE TABLE authWhitelistAlliances ( allianceID INT, allianceTicker VARCHAR(5), allianceName VARCHAR(255), datetime DATETIME, user VARCHAR(255), PRIMARY KEY ( allianceID ), KEY ( allianceTicker ), KEY ( allianceName ) )")))

;; Update tables

(define (sql-auth-update-whitelist-corporations lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO authWhitelistCorporations VALUES (?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE datetime=?,user=?"
		     (customCorporations-id (sql-whitelist-content x))
		     (customCorporations-ticker (sql-whitelist-content x))
		     (customCorporations-name (sql-whitelist-content x))
		     (sql-whitelist-datetime x)
		     (sql-whitelist-username x)
		     (sql-whitelist-datetime x)
		     (sql-whitelist-username x)))
	    lst))

(define (sql-auth-update-whitelist-alliances lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO authWhitelistAlliances VALUES (?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE datetime=?,user=?"
		     (customAlliances-id (sql-whitelist-content x))
		     (customAlliances-ticker (sql-whitelist-content x))
		     (customAlliances-name (sql-whitelist-content x))
		     (sql-whitelist-datetime x)
		     (sql-whitelist-username x)
		     (sql-whitelist-datetime x)
		     (sql-whitelist-username x)))
	    lst))

(define (sql-auth-remove-whitelist-corporations lst)
  (for-each (lambda (x)
	      (query sqlc "DELETE FROM authWhitelistCorporations WHERE corporationID = ?"
		     (customCorporations-id (sql-whitelist-content x))))
	    lst))

(define (sql-auth-remove-whitelist-alliances lst)
  (for-each (lambda (x)
	      (query sqlc "DELETE FROM authWhitelistAlliances WHERE allianceID = ?"
		     (customAlliances-id (sql-whitelist-content x))))
	    lst))

;; Get entries from tables

(define (sql-auth-get-whitelist-corporations)
  (query-rows sqlc "SELECT corporationID,corporationTicker,corporationName,datetime,user FROM authWhitelistCorporations"))

(define (sql-auth-get-whitelist-alliances)
  (query-rows sqlc "SELECT allianceID,allianceTicker,allianceName,datetime,user FROM authWhitelistAlliances"))

;; Functions/Macros

;; Parse whitelist data

(define (parse-whitelist bind)
  (cond [(not (null? bind))
	 (filter-map (lambda (entry)
		       (match entry
			 [(pregexp "\\+\\s*(.+)" (list _ a)) (cons "+" a)]
			 [(pregexp "\\-\\s*(.+)" (list _ a)) (cons "-" a)]
			 [else #f]))
		     (string-split bind "\r\n"))]
	[else null]))

;; Sanitise whitelist data

(define-syntax (check-whitelist stx)
  (syntax-case stx ()
    [(_ #:type type #:user user lst)
     (with-syntax ([make-type (syntax->datum #'type)]
		   [make-bind (format-id #'type "maybe-~a" (syntax->datum #'type))]
		   [make-test (format-id #'type "parse-~a" (syntax->datum #'type))]
		   [make-parse (format-id #'type "custom~as" (string-titlecase (syntax->datum #'type)))])
       #'(let ([timestamp (srfi-date->sql-timestamp (current-date))])
	   (remove-duplicates
	    (filter-map (lambda (entry)
			  (let ([make-bind (make-test (cdr entry))])
			    (cond [(not (false? make-bind))
				   (sql-whitelist (car entry)
						  make-type
						  (sql-parse->struct make-bind #:struct make-parse)
						  timestamp
						  (if (null? user) "" user))]
				  [else #f])))
			lst)
	    #:key (lambda (x) (sql-whitelist-content x)))))]))

;; Write whitelist data

(define-syntax (write-whitelist stx)
  (syntax-case stx ()
    [(_ #:type type lst)
     (with-syntax ([make-type (syntax->datum #'type)]
		   [make-update (format-id #'type "sql-auth-update-whitelist-~as" (syntax->datum #'type))]
		   [make-remove (format-id #'type "sql-auth-remove-whitelist-~as" (syntax->datum #'type))])
       #'(begin
	   (make-update
	    (filter (lambda (x) (and (equal? (sql-whitelist-action x) "+")
				     (equal? (sql-whitelist-type x) make-type)))
		    lst))
	   (make-remove
	    (filter (lambda (x) (and (equal? (sql-whitelist-action x) "-")
				     (equal? (sql-whitelist-type x) make-type)))
		    lst))))]))

;; Check whether input is a member of any whitelist

(define (whitelist? input)
  (let ([maybe-alliance (parse-alliance input)]
	[maybe-corporation (parse-corporation input)])
    (number?
     (cond
      [(not (false? maybe-alliance))
       (query-maybe-value sqlc
			  "SELECT 1 FROM authWhitelistAlliances WHERE allianceID = ?" (vector-ref maybe-alliance 0))]
      [(not (false? maybe-corporation))
       (query-maybe-value sqlc
			  "SELECT 1 FROM authWhitelistCorporations WHERE corporationID = ?" (vector-ref maybe-corporation 0))]
      [else #f]))))
