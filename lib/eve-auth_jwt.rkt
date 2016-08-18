#lang racket

(require racket/date)
(require net/jwt)
(require (prefix-in req: web-server/http/request-structs))

(require "eve-sql_structs.rkt")

(provide (prefix-out auth: (all-defined-out)))

(define (create-token #:audiences audiences
		      #:issuer issuer
		      #:subject subject
		      #:username [name ""])
  (encode/sign "HS256"
	       (getenv "JWT_SECRET")
	       #:iss issuer
	       #:sub subject
	       #:aud audiences
	       #:exp (+ (current-seconds) 120)
	       #:iat (current-seconds)
	       #:nbf (current-seconds)
	       #:other (hasheq 'username name)))

(define (verify-token token
		      #:audiences audiences
		      #:issuer issuer)
  (verify-jwt (decode-jwt token)
	      "HS256"
	      (getenv "JWT_SECRET")
	      #:iss issuer
	      #:aud audiences))

(define (extract-data token)
  (cond
   [(and (VerifiedJWT? token)
	 (>= (current-seconds) (date->seconds (not-before token)))
	 (<= (current-seconds) (date->seconds (expiration-date token))))
    (recon-jwt (issuer token) (audiences token) (subject token) (claims-ref token 'username))]
   [else #f]))

(define (create-authorization-header token)
  (req:header #"Authorization"
	      (string->bytes/utf-8 (string-append "JWT " token))))

(define (extract-authorization-header lst)
  (let ([header-auth (req:headers-assq* #"authorization" lst)])
    (if header-auth
	(bytes->string/utf-8 (subbytes (req:header-value header-auth) 4))
	null)))
