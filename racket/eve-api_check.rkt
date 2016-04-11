#! /usr/bin/env racket
#lang racket

(require eve)

;; XML options

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; Define API polls

;; affiliation -> sql-ready list

(define (map-hash-parse-affiliation lst)
  (map (lambda (hash) (list
		       (hash-ref hash 'characterID)
		       (hash-ref hash 'characterName)
		       (hash-ref hash 'corporationID)
		       (hash-ref hash 'corporationName)
		       (hash-ref hash 'allianceID)
		       (hash-ref hash 'allianceName)))
       lst))

;; Exec

(sql-super-populate-affiliations)

(sql-super-update-affiliations
 (exec-limit-api-rate #:function hash-poll-affiliation
		      #:input (map number->string (sql-super-get-characterids))
		      #:digest map-hash-parse-affiliation
		      #:delay 30
		      #:limit 1500))
