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

;; Create moon goo tables

(sql-goo-create-raw)
(sql-goo-create-view)
(sql-goo-create-descriptive-view)

;; Create moon scan tables

(sql-moon-create-raw)
(sql-moon-create-pseudomaterialized-view)
(sql-moon-create-view)
(sql-moon-create-tasks)

(sql-moon-create-triggers)

;; Create type association view

(sql-type-create-associations)

;; Create tower kill tables

(sql-tower-create-raw)
(sql-tower-create-view)

(sql-tower-create-triggers)

;; Create citadel scan/kill tables

(sql-citadel-create-raw)
(sql-citadel-create-kill-raw)
(sql-citadel-create-id-table)
(sql-citadel-create-pseudomaterialized-view)
(sql-citadel-create-view)
(sql-citadel-create-kill-view)
(sql-citadel-create-delete)

(sql-citadel-create-triggers)
(sql-citadel-create-kill-triggers)

;; Create timerboard table

(timerboard-prepare-table)
(timerboard-create-view)

;; Create super canary tables

(sql-canary-create-corporations)
(sql-canary-create-alliances)

;; Create local scan character tables

(sql-character-create-raw)
