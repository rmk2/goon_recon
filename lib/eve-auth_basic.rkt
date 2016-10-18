#lang racket

(require db)
(require db/util/datetime)
(require srfi/19)

(require scrypt)
(require racket/random)
(require (only-in openssl/sha1
		  bytes->hex-string
		  hex-string->bytes))

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")

(provide (prefix-out auth: (all-defined-out)))

;; Create tables

(define (sql-auth-create-raw)
  (if (table-exists? sqlc "authBasic")
      #t
      (query-exec sqlc "CREATE TABLE authBasic ( user VARCHAR(255) NOT NULL, password VARCHAR(64) NOT NULL, salt VARCHAR(64) NOT NULL, date DATETIME NOT NULL, PRIMARY KEY (user) )")))

(define (sql-auth-create-mail-raw)
  (if (table-exists? sqlc "authBasicMail")
      #t
      (query-exec sqlc "CREATE TABLE authBasicMail ( user VARCHAR(255) NOT NULL, email VARCHAR(64) NOT NULL, salt VARCHAR(64) NOT NULL, date DATETIME NOT NULL, PRIMARY KEY (user) )")))

;; Insert data into tables

(define (sql-auth-insert-user hash)
  (query sqlc "INSERT IGNORE INTO authBasic VALUES (?, ?, ?, ?)"
	 (scrypt-hash-user hash)
	 (scrypt-hash-input hash)
	 (scrypt-hash-salt hash)
	 (srfi-date->sql-timestamp (current-date))))

(define (sql-auth-insert-mail hash)
  (query sqlc "INSERT IGNORE INTO authBasicMail VALUES (?, ?, ?, ?)"
	 (scrypt-hash-user hash)
	 (scrypt-hash-input hash)
	 (scrypt-hash-salt hash)
	 (srfi-date->sql-timestamp (current-date))))

;; Query tables

(define (sql-auth-get-user user)
  (let ([data (query-maybe-row sqlc "SELECT user,password,salt FROM authBasic WHERE user = ?" user)])
    (if (not (false? data))
	(call-with-values
	    (lambda ()
	      (vector->values data))
	  scrypt-hash)
	#f)))

(define (sql-auth-get-mail user)
  (let ([data (query-maybe-row sqlc "SELECT user,email,salt FROM authBasicMail WHERE user = ?"  user)])
    (if (not (false? data))
	(call-with-values
	    (lambda ()
	      (vector->values data))
	  scrypt-hash)
	#f)))

(define (sql-auth-get-user-full user)
  (let ([data (query-maybe-row sqlc "SELECT u.user,m.email,m.salt AS emailSalt,u.password,u.salt AS passwordSalt FROM authBasic AS u LEFT JOIN authBasicMail AS m ON u.user = m.user WHERE u.user = ?" user)])
    (if (not (false? data))
	(call-with-values
	    (lambda ()
	      (vector->values data))
	  scrypt-full)
	#f)))

;; Calculate scrypt hash+salt for input

(define (scrypt-input->hash input #:length [length 32] #:N [N 16] #:r [r 8] #:p [p 1])
  (let ([salt (crypto-random-bytes length)])
    (scrypt-hash null
		 (bytes->hex-string (scrypt input salt N r p length))
		 (bytes->hex-string salt))))

;; Check whether scrypt'ed input matches provided scrypt hash+salt

(define (scrypt-check-hash hash input #:length [length 32] #:N [N 16] #:r [r 8] #:p [p 1])
  (equal? (scrypt-hash-input hash)
	  (bytes->hex-string (scrypt input (hex-string->bytes (scrypt-hash-salt hash)) N r p length))))
