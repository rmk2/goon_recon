#lang racket

(require racket/serialize)

(require eve)
(require eve/eve-auth_sso)
(require eve/eve-whitelist_tools)

(require "common.rkt")

(provide (all-defined-out))

;; User registration via SSO

(define (exec-auth-token-request req #:type type)
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
		    (p (a 'href: (sso-request-auth-token (auth:create-token #:subject type)) (img 'src: (login-button)))))
	       (div 'id: "links"
		    (a 'target: "_blank"
		       'rel: "noopener noreferrer"
		       'href: "https://support.eveonline.com/hc/en-us/articles/205381192-Single-Sign-On-SSO-"
		       "Learn more about EVE Single-Sign-On (SSO)")
		    (a 'href: "/login" "Already registered?")))))
	out))))

  (send/back response-generator))

(define (exec-auth-token-response req)
  (let* ([sso-bindings (request-bindings req)]
	 [sso-state (auth:extract-data (auth:verify-token (extract-binding/single 'state sso-bindings)))]
	 [sso-code (extract-binding/single 'code sso-bindings)]
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
	 [affiliation-data (car (map-character-hash->struct parsed-affiliation))])

    (sql-character-update-ids (list affiliation-data))

    (cond
     [(and (recon-jwt? sso-state) (equal? (recon-jwt-subject sso-state) "register"))
      (let ([token (auth:create-token #:subject "auth" #:username (sql-character-name affiliation-data))])
	(redirect-to "register"
		     #:headers (list (auth:create-authorization-header token)
				     (cookie->header (make-cookie "registration_token" token #:max-age 600 #:path "/")))))]
     [(and (recon-jwt? sso-state) (equal? (recon-jwt-subject sso-state) "login"))
      (let* ([group (cond [(whitelist? (sql-character-corporationid affiliation-data)) "corporation"]
			  [(whitelist? (sql-character-allianceid affiliation-data)) "alliance"]
			  [else "public"])]
	     [token (auth:create-token #:subject group #:username (sql-character-name affiliation-data))])
	(redirect-to "dscan"
		     #:headers (list (auth:create-authorization-header token)
				     (cookie->header (make-cookie "access_token" token #:max-age 600 #:path "/")))))]
     [else (redirect-to "register")])))
