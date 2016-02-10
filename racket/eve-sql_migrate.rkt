#! /usr/bin/env racket
#lang racket

(require eve)
(require "eve-api_check.rkt")

(define input-data (unique-car (input-map-split (edis-data)) second))
(define check-data (parse-data (api-check-result (edis-list polled-data))))

(eve-create-tables)

(sql-replace-killmails input-data)
(sql-replace-characters check-data)

(sql-create-view)
