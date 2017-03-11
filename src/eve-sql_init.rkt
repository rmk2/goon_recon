#! /usr/bin/env racket
#lang racket

(require eve)
(require eve/eve-whitelist_tools)

;; Initiate and populate alliance table

(sql-create-alliances)
(sql-replace-alliances (api-fetch-alliances))

;; Create corporation table

(sql-corporation-create-raw)
(sql-corporation-create-input)

;; Create corporation affiliation table

(sql-corporation-create-affiliations)

(sql-corporation-create-triggers)

;; Create local scan character tables

(sql-character-create-raw)

(sql-character-create-triggers)

;; Create super tracking/watchlist tables

(sql-super-create-raw)
(sql-super-create-view)
(sql-super-create-affiliations)
(sql-super-create-latest-pseudomaterialized-view)
(sql-super-create-watchlist)

(sql-super-latest-create-triggers)

;; Create moon goo tables

(sql-goo-create-raw)
(sql-goo-create-view)
(sql-goo-create-descriptive-view)

;; Create tower kill tables

(sql-tower-create-raw)
(sql-tower-create-view)

(sql-tower-create-triggers)

;; Create moon scan tables

(sql-moon-create-raw)
(sql-moon-create-pseudomaterialized-view)
(sql-moon-create-view)
(sql-moon-create-tasks)

(sql-moon-create-triggers)

;; Create type association view

(sql-type-create-associations)

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

;; Create scan username table

(sql-scan-create-users)

;; Create sovereignty tables

(sov:sql-sov-create-campaigns-raw)
(sov:sql-sov-create-campaigns-view)
(sov:sql-sov-create-timerboard-view)

;; Create super canary tables

(sql-canary-create-corporations)
(sql-canary-create-alliances)

(sql-canary-create-watchlist-alliances)
(sql-canary-create-watchlist-corporations)

;; Create local auth tables

(auth:sql-auth-create-user-raw)

(auth:sql-auth-create-group-ids)
(auth:sql-auth-insert-group-ids)

(auth:sql-auth-create-groups-raw)
(auth:sql-auth-create-groups-view)

(auth:sql-auth-create-user-characters)

(auth:sql-auth-create-triggers)

;; Create whitelist tables

(sql-auth-create-whitelist-corporations)
(sql-auth-create-whitelist-alliances)
