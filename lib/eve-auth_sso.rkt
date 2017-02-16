#lang racket

(require db)
(require file/gunzip)
(require json)
(require net/url)
(require webapi/oauth2)

(require "eve-sql_main.rkt")
(require "eve-sql_structs.rkt")

(provide (all-defined-out))

;; SQL

(define (sql-oauth2-create-tokens)
  (if (table-exists? sqlc "authTokens")
      #t
      (query-exec sqlc "CREATE TABLE authTokens ( characterID INT NOT NULL, characterName VARCHAR(255) NOT NULL, refreshToken VARCHAR(255), datetime DATETIME, PRIMARY KEY ( characterID ) )")))

(define (sql-oauth2-update-tokens lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO authTokens VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE refreshToken=?,datetime=?"
		     (sql-sso-auth-characterid x)
		     (sql-sso-auth-charactername x)
		     (sql-sso-auth-refresh-token x)
		     (sql-sso-auth-datetime x)
		     (sql-sso-auth-refresh-token x)
		     (sql-sso-auth-datetime x)))
	    lst))

(define-syntax sql-oauth2-get-tokens
  (syntax-rules ()
    ((_ arg) (cond
	      [(number? arg)
	       (query-maybe-row sqlc "SELECT characterID,characterName,refreshToken,datetime FROM authTokens WHERE characterID = ?" arg)]
	      [(string? arg)
	       (query-maybe-row sqlc "SELECT characterID,characterName,refreshToken,datetime FROM authTokens WHERE characterName = ?" arg)]))))

;; SSO login button image (base64 encoded)
;; embed via (img (src login-button))

(define (login-button)
  "data:image/png;base64,
  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
j7o9/F12/h8TB8xJk2exoQAAAABJRU5ErkJggg==")

;; Parameters

(define code-redirect-uri (getenv "SSO_CODE_REDIRECT"))
(define token-redirect-uri (getenv "SSO_TOKEN_REDIRECT"))

(define code-request-scopes
  (list (string-join (list
		      "characterAccountRead"
		      "characterContactsRead"
		      "characterLocationRead"
		      "characterSkillsRead"
		      "characterStatsRead"
		      "characterWalletRead"
		      "publicData"))))

;; EVE Online SSO definitions

(define eve-sso-server
  (oauth2-auth-server #:auth-url "https://login.eveonline.com/oauth/authorize"
		      #:token-url "https://login.eveonline.com/oauth/token"
		      #:tokeninfo-url "https://login.eveonline.com/oauth/verify"))

(define eve-sso-code-client
  (oauth2-client #:id (getenv "SSO_CODE_ID")
		 #:secret (getenv "SSO_CODE_SECRET")))

(define eve-sso-token-client
  (oauth2-client #:id (getenv "SSO_TOKEN_ID")
		 #:secret (getenv "SSO_TOKEN_SECRET")))

;; SSO functions

(define (sso-request-auth-code [state "0"])
  (send eve-sso-server
	get-auth-request-url
	#:client eve-sso-code-client
	#:scopes code-request-scopes
	#:redirect-uri code-redirect-uri
	#:state state
	#:extra-parameters '((response_type . "code"))))

(define (sso-request-auth-token [state "0"])
  (send eve-sso-server
	get-auth-request-url
	#:client eve-sso-token-client
	#:scopes '("")
	#:redirect-uri token-redirect-uri
	#:state state
	#:extra-parameters '((response_type . "token"))))

(define (sso-auth-code->tokens code)
  (oauth2/auth-code eve-sso-server
		    eve-sso-code-client
		    code
		    #:redirect-uri code-redirect-uri))

(define (sso-auth-token->access-token code)
  (oauth2/auth-code eve-sso-server
		    eve-sso-token-client
		    code
		    #:redirect-uri token-redirect-uri))

(define (sso-refresh-token->auth-token refresh-token)
  (send (oauth2/refresh-token eve-sso-server
			      eve-sso-code-client
			      refresh-token)
	get-access-token))

;; Authenticated CREST

(define-syntax json-api-auth
  (syntax-rules (:gzip :plain)
    ((_ :plain str token)
     (bytes->jsexpr
      (call/input-url (string->url str)
		      get-pure-port
		      port->bytes
		      (list (format "Authorization: Bearer ~a" token)
			    "User-Agent: ryko@rmk2.org"))))
    ((_ :gzip str token)
     (bytes->jsexpr
      (call/input-url (string->url str)
		      get-pure-port
		      (lambda (input) (call-with-output-bytes (lambda (x) (gunzip-through-ports input x))))
		      (list (format "Authorization: Bearer ~a" token)
			    "Accept-Encoding: gzip"
			    "User-Agent: ryko@rmk2.org"))))
    ((_ str token) (json-api-auth :plain str token))))


