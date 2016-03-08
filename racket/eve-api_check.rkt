#! /usr/bin/env racket
#lang racket

(require eve)

;; XML options

(collapse-whitespace #t)
(xexpr-drop-empty-attributes #f)
(permissive-xexprs #t)

;; Define API polls

(define chunk-size (make-parameter 90))
(define query-limit (make-parameter 2500))

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

(sql-super-update-affiliations
 (map-hash-parse-affiliation
  (exec-limit-api-rate #:function hash-poll-affiliation
		       #:input (map number->string (sql-super-get-characterids)))))
