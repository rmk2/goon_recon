#lang racket/base

(require db)

(provide (all-defined-out))

(define sqlc (virtual-connection
	      (connection-pool
	       (lambda ()
		 (mysql-connect #:user "eve"
				#:database "eve_sde"
				#:password (getenv "MYSQL_PASSWORD")
                #:server (getenv "MYSQL_HOST")
                #:port 3306))
	       #:max-idle-connections 5)))
