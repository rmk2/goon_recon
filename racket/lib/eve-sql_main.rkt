#lang racket/base

(require db)

(provide (all-defined-out))

(define sqlc (mysql-connect #:user "eve"
			    #:database "eve_sde"
			    #:password "q+WK9nnGO3EWWZJQaxO8Iv55CdLRACAP"))
