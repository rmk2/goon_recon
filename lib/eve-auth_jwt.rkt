#lang racket

(require racket/date)
(require net/jwt)
(require (prefix-in req: web-server/http/request-structs))

(require "eve-sql_structs.rkt")

(provide (prefix-out auth: (all-defined-out)))

(define jwt-secret (make-parameter (getenv "JWT_SECRET")))
(define jwt-audiences (make-parameter (getenv "JWT_AUDIENCES")))
(define jwt-issuer (make-parameter (getenv "JWT_ISSUER")))

(define (create-token #:secret [secret (jwt-secret)]
		      #:audiences [audiences (jwt-audiences)]
		      #:issuer [issuer (jwt-issuer)]
		      #:subject subject
		      #:username [name ""]
		      #:expiration [expiration 600])
  (encode/sign "HS256"
	       secret
	       #:iss issuer
	       #:sub subject
	       #:aud audiences
	       #:exp (+ (current-seconds) expiration)
	       #:iat (current-seconds)
	       #:nbf (current-seconds)
	       #:other (hasheq 'username name)))

(define (verify-token token
		      #:secret [secret (jwt-secret)]
		      #:audiences [audiences (jwt-audiences)]
		      #:issuer [issuer (jwt-issuer)])
  (decode/verify token
		 "HS256"
		 secret
		 #:iss issuer
		 #:aud audiences))

(define (extract-data token)
  (cond
   [(and (VerifiedJWT? token)
	 (>= (current-seconds) (date->seconds (not-before token)))
	 (<= (current-seconds) (date->seconds (expiration-date token))))
    (recon-jwt (issuer token) (audiences token) (expiration-date token) (subject token) (claims-ref token 'username))]
   [else #f]))

(define (create-authorization-header token)
  (req:header #"X-Auth"
	      (string->bytes/utf-8 (string-append "JWT " token))))

(define (extract-authorization-header lst)
  (let ([header-auth (req:headers-assq* #"x-auth" lst)])
    (cond [(req:header? header-auth)
	   (bytes->string/utf-8 (subbytes (req:header-value header-auth) 4))]
	  [(list? header-auth) 
	   (let ([header-filter (filter (lambda (x) (bytes=? #"JWT" (req:header-value (subbytes x 0 3)))) header-auth)])
	     (bytes->string/utf-8 (subbytes (req:header-value header-filter) 4)))]
	  [else null])))

(define-syntax try-authorization-header
  (syntax-rules (:subject :username)
    ((_ req) (let* ([auth-header (extract-authorization-header (req:request-headers/raw req))]
		    [auth-try (if (null? auth-header)
				  #f
				  (extract-data
				   (verify-token auth-header)))])
	       auth-try))
    ((_ :subject req) (let ([auth-subject (try-authorization-header req)])
			(cond [(not (false? auth-subject)) (recon-jwt-subject auth-subject)]
			      [else null])))
    ((_ :username req) (let ([auth-username (try-authorization-header req)])
			 (cond [(not (false? auth-username)) (recon-jwt-username auth-username)]
			       [else null])))))
