#! /usr/bin/env racket
#lang racket

(require eve)

;; Initiate and populate alliance table

(sql-create-alliances)
(sql-replace-alliances (api-fetch-alliances))

;; Create corporation affiliation table

(sql-corporation-create-affiliations)

(sql-corporation-create-triggers)

;; Create super tracking/watchlist tables

(sql-super-create-affiliations)
(sql-super-create-raw)
(sql-super-create-view)
(sql-super-create-latest)
(sql-super-create-watchlist)

;; Create moon scan tables

(sql-moon-create-raw)
(sql-moon-create-pseudomaterialized-view)
(sql-moon-create-view)
(sql-moon-create-tasks)

(sql-moon-create-triggers)

;; Create moon goo tables

(sql-goo-create-raw)
(sql-goo-create-view)

;; Create type association view

(sql-type-create-associations)

;; Create tower kill tables

(sql-tower-create-raw)
(sql-tower-create-view)

(sql-tower-create-triggers)

;; Create timerboard table

(timerboard-prepare-table)

;; Create super canary tables

(sql-canary-create-corporations)
(sql-canary-create-alliances)
