#!/usr/bin/env racket
#lang racket

(require eve)

(define (sql-corporation-find-duplicate ticker)
  (vector->values
   (last
    (query-rows sqlc "SELECT corporationID FROM customCorporations WHERE corporationTicker = ?"
		ticker))))

(define (sql-corporation-delete-duplicate ticker keeper)
  (query-exec sqlc "DELETE FROM customCorporations WHERE corporationTicker = ? AND corporationID != ?"
	      ticker
	      keeper))

(define (corporation-delete-duplicate-ticker ticker)
  (sql-corporation-delete-duplicate ticker (sql-corporation-find-duplicate ticker)))

(for-each (lambda (corp)
	    (corporation-delete-duplicate-ticker corp))
	  (map vector->values
	       (query-rows sqlc (string-append "SELECT corporationTicker "
					       "FROM "
					       "( SELECT corporationTicker,COUNT(corporationTicker) AS tickercount "
					       "FROM customCorporations GROUP BY corporationTicker ) AS sub "
					       "WHERE sub.tickercount > 1"))))
