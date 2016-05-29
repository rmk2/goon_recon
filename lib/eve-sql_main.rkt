#lang racket/base

(require db)

(provide (all-defined-out))

(define sqlc (virtual-connection
	      (connection-pool
	       (lambda ()
		 (mysql-connect #:user "eve"
				#:database "eve_sde"
				#:password "q+WK9nnGO3EWWZJQaxO8Iv55CdLRACAP"))
	       #:max-idle-connections 5)))
