#lang racket

(require db)

(require "eve-sql_main.rkt")
(require "eve-string_tools.rkt")

(provide (all-defined-out))

(define (parse-moondata lst)
  (query-maybe-row sqlc (string-append
			 "SELECT corporationTicker,allianceTicker,datetime,moonType "
			 "FROM moondata WHERE systemName = ? AND planet = ? AND moon = ?")
		   (first lst)
		   (second lst)
		   (third lst)))

(define-syntax money-moon?
  (syntax-rules ()
    ((_ arg) (cond
	      [(list? arg) (if (parse-moondata arg) #t #f)]
	      [(string? arg) (if (parse-moondata (split-moon-display arg)) #t #f)]
	      [else #f]))))
