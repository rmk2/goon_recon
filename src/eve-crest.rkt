#! /usr/bin/env racket
#lang racket

(require eve)

(let ([data (sov:crest-sov-get-campaigns)])
  (if (null? data)
      (exit)
      (begin
	(sov:sql-sov-prepare-campaigns-raw)
	(sov:sql-sov-update-campaigns-raw data))))
