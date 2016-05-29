#! /usr/bin/env racket
#lang racket

(require eve)
(require "eve-api_check.rkt")

(sql-create-supers)
(sql-create-alliances)

(sql-replace-killmails (unique-car (input-map-split (edis-data)) second))
(sql-replace-characters (parse-data (api-check-result (edis-list polled-data))))
(sql-replace-alliances (api-fetch-alliances))

(sql-create-view)
