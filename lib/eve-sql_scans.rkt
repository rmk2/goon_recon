#lang racket

(require db)

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")

(provide (all-defined-out))

;; SQL

(define (sql-scan-create-users)
  (if (table-exists? sqlc "scanUsers")
      #t
      (query-exec sqlc "CREATE TABLE scanUsers ( scanID VARCHAR(64) NOT NULL, username VARCHAR(255), type VARCHAR(25), datetime DATETIME, PRIMARY KEY ( scanID ) )")))

(define (sql-scan-update-users lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO scanUsers VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE username=?,type=?,datetime=?"
		     (sql-scan-id x)
		     (sql-scan-user x)
		     (sql-scan-type x)
		     (sql-scan-datetime x)
		     (sql-scan-user x)
		     (sql-scan-type x)
		     (sql-scan-datetime x)))
	    lst))
