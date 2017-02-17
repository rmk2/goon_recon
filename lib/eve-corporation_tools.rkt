#lang racket

(require db)

(require "eve-sql_main.rkt")
(require "eve-api_tools.rkt")
(require "eve-esi_tools.rkt")

(provide (all-defined-out))

;; Select unknown corporations from towerKillRaw/citadelKillRaw

(define (query-kill-unknown-corporations)
  (flatten
   (filter-not empty?
	       (list*
		(query-list sqlc "SELECT towerKillRaw.corporationID FROM towerKillRaw LEFT JOIN customCorporations ON customCorporations.corporationID = towerKillRaw.corporationID WHERE corporationName IS NULL")
		(query-list sqlc "SELECT citadelKillRaw.corporationID FROM citadelKillRaw LEFT JOIN customCorporations ON customCorporations.corporationID = citadelKillRaw.corporationID WHERE corporationName IS NULL")))))

;; Select unknown corporations from moonScanRaw/citadelScanRaw

(define (query-scan-unknown-corporations)
  (flatten
   (filter-not empty?
	       (list*
		(query-list sqlc "SELECT DISTINCT corporationTicker FROM moonScanRaw WHERE NOT EXISTS ( SELECT * FROM customCorporations AS corp WHERE moonScanRaw.corporationTicker = corp.corporationTicker )")
		(query-list sqlc "SELECT DISTINCT corporationTicker FROM citadelScanRaw WHERE NOT EXISTS ( SELECT * FROM customCorporations AS corp WHERE citadelScanRaw.corporationTicker = corp.corporationTicker )")))))

;; Translate corporationTicker->corporationID from moonScanRaw/citadelScanRaw

(define (query-scan-unknown-corporations->ids)
  (exec-limit-api-rate #:function (lambda (ticker) (filter-map esi-try-corporation ticker))
		       #:input (query-scan-unknown-corporations)
		       #:delay 1
		       #:limit 30))

;; Select all entries from customCorporationInput (that aren't zero)

(define (query-input-unknown-corporations)
  (query-list sqlc "SELECT corporationID FROM customCorporationInput WHERE corporationID != 0"))

;; Combine all unknown corporations

(define (query-unknown-corporations-union)
  (append (query-kill-unknown-corporations)
	  (query-scan-unknown-corporations->ids)
	  (query-input-unknown-corporations)))
