#! /usr/bin/env racket
#lang racket

(require eve)

(let ([data (timers:query-sovereignty-timers)])
  (if (null? data)
      (exit)
      (begin
	(timers:timerboard-prepare-table)
	(timers:timerboard-replace data))))
