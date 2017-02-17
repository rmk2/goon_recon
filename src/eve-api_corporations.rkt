#!/usr/bin/env racket
#lang racket

(require eve)

(define (general-unknown-poll lst)
  (exec-limit-api-rate #:function esi-hash-poll-corporation
		       #:input (map number->string lst)
		       #:delay 1
		       #:digest esi-hash-parse-corporation
		       #:limit 30))

;; Exec

(sql-corporation-update-corporations
 (general-unknown-poll
  (query-unknown-corporations-union)))
