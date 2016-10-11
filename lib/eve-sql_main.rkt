#lang racket/base

(require db)

(provide (all-defined-out))

(define sqlc (virtual-connection
	      (connection-pool
	       (lambda ()
		 (mysql-connect #:user "eve"
				#:database "eve_sde"
				#:password (getenv "MYSQL_PASSWORD")))
	       #:max-idle-connections 5)))
