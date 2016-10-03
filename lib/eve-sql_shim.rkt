#lang racket

(require db)

(require "eve-sql_main.rkt")

(provide (all-defined-out))

(define (sql-revive-connection)
  (if (false? (connected? sqlc))
      (query-exec sqlc "SELECT NOW()")
      void))
