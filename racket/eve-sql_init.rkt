#! /usr/bin/env racket
#lang racket

(require eve)

;; Initiate and populate alliance table

(sql-create-alliances)
(sql-replace-alliances (api-fetch-alliances))

;; Create corporation affiliation table

(sql-corporation-create-affiliations)

;; Create super tracking/watchlist tables

(sql-super-create-affiliations)
(sql-super-create-raw)
(sql-super-create-view)
(sql-super-create-latest)
(sql-super-create-watchlist)

;; Create moon scan tables

(sql-moon-create-raw)
(sql-moon-create-view)

;; Create timerboard table

(timerboard-prepare-table)

;; Create super canary tables

(sql-canary-create-corporations)
(sql-canary-create-alliances)
