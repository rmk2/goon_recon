#! /usr/bin/env racket
#lang racket

(require eve)
(require "eve-api_check.rkt")
(require "eve-sql_supers.rkt")

(define input-data (unique-car (input-map-split (edis-data)) second))
(define check-data (parse-data (edis-result)))

(eve-create-tables)

(sql-replace-killmails input-data)
(sql-replace-characters check-data)

(sql-create-view)
