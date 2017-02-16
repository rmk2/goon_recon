#lang racket

(require racket/serialize)

(require eve)
(require eve/eve-auth_sso)

(require "common.rkt")
(require "register.rkt")

(provide (all-defined-out))

;; User registration via SSO

(define (exec-auth-token-pre req)
  (cond [(not (false? (try-auth-cookie req #:type "access_token")))
	 (redirect-to "login")]
	[(not (false? (try-auth-cookie req #:type "registration_token")))
	 (exec-register req)]
	[(not (null? (request-bindings req)))
	 (exec-auth-token-response req)]
	[else (exec-auth-token-request req)]))

(define (exec-auth-token-request req)
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (output:create-html-head
	  #:title "User Identification (SSO)"
	  #:tablesorter #f
	  #:navigation #f
	  #:forms #t)
	 (body
	  (div 'id: "content"
	       (h1 "User Identification (SSO)")
	       (div 'class: "info"
		    (p "Continue to EVE SSO to identify yourself:")
		    (p (a 'href: (sso-request-auth-token) (img 'src: (login-button)))))
	       (div 'id: "links"
		    (a 'target: "_blank"
		       'rel: "noopener noreferrer"
		       'href: "https://support.eveonline.com/hc/en-us/articles/205381192-Single-Sign-On-SSO-"
		       "Learn more about EVE Single-Sign-On (SSO)")
		    (a 'href: "login" "Already registered?")))))
	out))))

  (send/back response-generator))

(define (exec-auth-token-response req)
  (let* ([sso-code (extract-binding/single 'code (request-bindings req))]
	 [sso-response (sso-auth-token->access-token sso-code)]
	 [sso-access (send sso-response get-access-token)]
	 [data (json-api-auth "https://login.eveonline.com/oauth/verify" sso-access)]
	 [parsed-data
	  (apply sql-sso-auth (list (hash-ref data 'CharacterID)
				    (hash-ref data 'CharacterName)
				    ""
				    (srfi-date->sql-timestamp (current-date))))]
	 [parsed-affiliation
	  (hash-poll-affiliation (list (number->string (sql-sso-auth-characterid parsed-data))))]
	 [token
	  (auth:create-token #:subject "auth"
			     #:username (sql-sso-auth-charactername parsed-data))])

    (sql-character-update-ids (map-character-hash->struct parsed-affiliation))

    (redirect-to "register" #:headers (list (auth:create-authorization-header token)
					    (cookie->header (make-cookie "registration_token" token #:max-age 600 #:path "/"))))))
