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

(define (sql-auth-create-user-raw)
  (if (table-exists? sqlc "authBasic")
      #t
      (query-exec sqlc "CREATE TABLE authBasic ( user VARCHAR(255) NOT NULL, password VARCHAR(64) NOT NULL, salt VARCHAR(64) NOT NULL, datetime DATETIME NOT NULL, PRIMARY KEY (user) )")))

(define (sql-auth-create-mail-raw)
  (if (table-exists? sqlc "authBasicMail")
      #t
      (query-exec sqlc "CREATE TABLE authBasicMail ( user VARCHAR(255) NOT NULL, email VARCHAR(64) NOT NULL, salt VARCHAR(64) NOT NULL, datetime DATETIME NOT NULL, PRIMARY KEY (user) )")))

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

;; Group handling

(define (sql-auth-create-groups-raw)
  (if (table-exists? sqlc "authBasicGroups")
      #t
      (query-exec sqlc "CREATE TABLE authBasicGroups ( user VARCHAR(255) NOT NULL, groupID INT NOT NULL DEFAULT 1, datetime DATETIME NOT NULL DEFAULT '0000-00-00 00:00:00', PRIMARY KEY (user) )")))

(define (sql-auth-create-groups-view)
  (if (table-exists? sqlc "authBasicGroupsView")
      #t
      (query-exec sqlc "CREATE VIEW authBasicGroupsView AS SELECT u.user,u.groupID,g.groupName,u.datetime FROM authBasicGroups AS u LEFT JOIN authGroups AS g ON u.groupID=g.groupID")))

(define (sql-auth-create-group-ids)
  (if (table-exists? sqlc "authGroups")
      #t
      (query-exec sqlc "CREATE TABLE authGroups ( groupID INT NOT NULL, groupName VARCHAR(255) NOT NULL, PRIMARY KEY (groupID) )")))

(define (sql-auth-insert-group-ids)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO authGroups VALUES (?, ?)"
		     (car x)
		     (cdr x)))
	    '((1 . "public")
	      (4 . "alliance")
	      (8 . "corporation")
	      (32 . "recon")
	      (64 . "recon-l")
	      (256 . "admin")
	      (1024 . "owner"))))

(define (sql-auth-update-groups lst)
  (for-each (lambda (x)
	      (query sqlc "UPDATE authBasicGroups SET groupID=?,datetime=? WHERE user=?"
		     (second x)
		     (srfi-date->sql-timestamp (current-date))
		     (first x)))
	    lst))

(define (sql-auth-get-groups)
  (query-rows sqlc "SELECT user,groupID FROM authBasicGroups ORDER BY user"))

(define-syntax sql-auth-get-user-group
  (syntax-rules (:id :name)
    ((_ user) (query-maybe-value sqlc "SELECT groupID FROM authBasicGroups WHERE user = ?" user))
    ((_ :id user) (sql-auth-get-user-group user))
    ((_ :name user) (query-maybe-value sqlc "SELECT groupName FROM authBasicGroupsView WHERE user = ?" user))))

;; Define SQL triggers for authBasicGroups

(define (sql-auth-trigger-insert)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER insert_authBasicGroups AFTER INSERT ON authBasic "
		    "FOR EACH ROW BEGIN "
		    "INSERT INTO authBasicGroups "
		    "SELECT "
		    "NEW.user,1,NEW.datetime "
		    "FROM authBasic "
		    "WHERE authBasic.user=NEW.user; "
		    "END;")))

(define (sql-auth-trigger-delete)
  (query-exec sqlc (string-append
		    "CREATE TRIGGER delete_authBasicGroups AFTER DELETE ON authBasic "
		    "FOR EACH ROW BEGIN "
		    "DELETE authBasicGroups FROM authBasicGroups "
		    "WHERE authBasicGroups.user=OLD.user; "
		    "END;")))

(define (sql-auth-trigger-protect) ;; Protect users in the "owner" group from being demoted
  (query-exec sqlc (string-append
		    "CREATE TRIGGER protect_authBasicGroups BEFORE UPDATE ON authBasicGroups "
		    "FOR EACH ROW "
		    "IF OLD.groupID >= 1024 THEN "
		    "SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Cannot update locked record';"
		    "END IF;")))

(define (sql-auth-create-triggers)
  (begin
    (query-exec sqlc "DROP TRIGGER IF EXISTS insert_authBasicGroups")
    (sql-auth-trigger-insert)
    (query-exec sqlc "DROP TRIGGER IF EXISTS delete_authBasicGroups")
    (sql-auth-trigger-delete)
    (query-exec sqlc "DROP TRIGGER IF EXISTS protect_authBasicGroups")
    (sql-auth-trigger-protect)))
