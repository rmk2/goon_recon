#! /usr/bin/env racket
#lang racket

(require tasks)
(require racket/async-channel)

(require eve)

(require (prefix-in corp: "eve-api_corporations.rkt"))

;; Channels

(define control (make-channel))
(define character-queue (make-async-channel))
(define corporation-queue (make-async-channel))

;; Workers

(define (log-output str #:id id)
  (let ([out (current-output-port)])
    (fprintf out "[Thread ~a] ~a → ~a~n" id (date->string (current-date) "~5") str)
    (flush-output out)))

(define (create-worker [id (gensym)])
  (thread (lambda () (let loop ([th (current-thread)])
		       (sync/timeout 120
				     (wrap-evt character-queue
					       (lambda (msg) (begin (log-output 'characters #:id id)
								    (exn-wrapper (characters-api-helper msg))
								    (sleep 1)))) ;; Limit API accessing speed
				     (wrap-evt corporation-queue
					       (lambda (msg) (begin (log-output 'corporations #:id id)
								    (exn-wrapper (corporations-api-helper msg))
								    (sleep 1)))) ;; Limit API accessing speed
				     (wrap-evt control
					       (lambda (msg) (begin (log-output msg #:id id)
								    (match msg
								      ['exit (kill-thread th)]
								      ['alliances (exn-wrapper (alliances-api-helper))]
								      ['citadels (exn-wrapper (citadels-api-helper))]
								      ['sovereignty (exn-wrapper (sovereignty-api-helper))]
								      ['supers (exn-wrapper (supers-api-helper))]
								      ['towers (exn-wrapper (towers-api-helper))])))))
		       (loop th)))))

;; Laziness helper function(s)

(define (hours->seconds hours) (* hours 60 60))

(define (offset-hours->seconds hours #:offset [offset 10])
  (+ (hours->seconds hours) (random (+ offset 1))))

;; Exception handling

(define-syntax (exn-wrapper stx)
  (syntax-case stx ()
    [(_ func)
     #'(with-handlers ([exn:fail? (lambda (e) (void))])
	 func)]
    [(_ func param)
     #'(with-handlers ([exn:fail? (lambda (e) (void))])
	 (func param))]))

;; SQL get last ID for a given table

(define/contract (sql-get-latest-id table)
  (-> string? (or/c number? false/c))
  (if (table-exists? sqlc table)
      (query-maybe-value sqlc (format "SELECT MAX(killID) FROM ~a" table))
      #f))

;; zKillboard killmail poll helper

(define (zkill-poll-helper groupids
			   #:table table
			   #:losses [run-losses? #t]
			   #:location [location? #t]
			   #:moons [moons? #f])
  (let ([data (digest:poll-url #:date null
			       #:groups groupids
			       #:kills (if run-losses? #f #t)
			       #:losses (if run-losses? #t #f)
			       #:id (sql-get-latest-id table))])
    (if (not (list? data))
	null
	(digest:parse-kills #:attackers (if run-losses? #f #t)
			    #:raw #t
			    #:location location?
			    #:moons moons?
			    #:groups groupids
			    data))))

;; Character updater

(define (sql-local-get-characterids)
  (map number->string (query-list sqlc "SELECT characterID FROM customCharacters")))

(define (characters-api-helper lst)
  (sql-character-update-ids
   (exec-limit-api-rate #:function hash-poll-affiliation
			#:input lst
			#:digest map-character-hash->struct
			#:delay 1
			#:limit (+ (chunk-size) 5))))

;; (define queue-characters-auto
;;   (schedule-recurring-task (lambda () (async-channel-put character-queue "Characters!")) 70))

(define queue-characters-auto
  (schedule-recurring-task
   (lambda ()
     (for-each (lambda (lst) (async-channel-put character-queue lst))
	       (split-list (sql-local-get-characterids) (chunk-size))))
   (hours->seconds 6)))

;; Corporation updater

(define (sql-unknown-corporations-union)
  (remove-duplicates
   (append
    (append-map (lambda (alliance) (corp:parse-corporationids (corp:crest-poll-allianceid (parse-alliance :id alliance))))
		(corp:extract-corporationids (corp:query-scan-unknown-corporations)))
    (map number->string
	 (append (corp:query-kill-unknown-corporations)
		 (corp:query-input-unknown-corporations))))))

(define (corporations-api-helper lst)
  (sql-corporation-update-corporations
   (exec-limit-api-rate #:function esi-hash-poll-corporation
			#:input lst
			#:digest esi-hash-parse-corporation
			#:delay 1
			#:limit 30)))

;; (define queue-corporations-auto
;;   (schedule-recurring-task (lambda () (async-channel-put corporation-queue "Corporations!")) 50))

(define query-corporations-auto
  (schedule-recurring-task
   (lambda ()
     (for-each (lambda (lst) (async-channel-put corporation-queue lst))
	       (split-list (sql-unknown-corporations-union) (chunk-size))))
   (hours->seconds 4)))

;; Alliance updater

(define (alliances-api-helper)
  (sql-replace-alliances
   (api-fetch-alliances)))

(define poll-alliances-auto
  (schedule-recurring-task (lambda () (channel-put control 'alliances)) (hours->seconds 12)))

;; Citadel updater

(define (parse-citadel-data lst)
  (filter (lambda (x) (positive? (sql-killmail-location x))) lst))

(define (citadels-api-helper)
  (sql-citadel-update-kill
   (parse-citadel-data
    (zkill-poll-helper '("1404" "1657") #:table "citadelKillRaw" #:losses #t))))

(define poll-citadels-auto
  (schedule-recurring-task (lambda () (channel-put control 'citadels)) (offset-hours->seconds 2 #:offset 60)))

;; Sovereignty updater

(define (sovereignty-api-helper)
  (begin
    (sov:sql-sov-prepare-campaigns-raw)
    (sov:sql-sov-update-campaigns-raw
     (sov:crest-sov-get-campaigns))))

(define poll-sovereignty-auto
  (let* ([interval 600]
	 [time (current-date)]
	 [time-diff (remainder (+ (* (date-minute time) 60) (date-second time)) interval)])
    (if (zero? time-diff)
	(schedule-recurring-task (lambda () (channel-put control 'sovereignty)) (hours->seconds 1/6))
	(schedule-delayed-task
	 (lambda () (schedule-recurring-task (lambda () (channel-put control 'sovereignty)) (hours->seconds 1/6)))
	 (- interval time-diff)))))

;; Super updater

(define (supers-api-helper)
  (begin
    (sql-super-insert-killmails
     (append
      (map (lambda (x) (begin (set-sql-killmail-eventtype! x "Kill") x))
	   (zkill-poll-helper '("30" "659") #:table "intelSuperRaw" #:losses #f))
      (map (lambda (x) (begin (set-sql-killmail-eventtype! x "Loss") x))
	   (zkill-poll-helper '("30" "659") #:table "intelSuperRaw" #:losses #t))))
    (sql-super-populate-affiliations)))

(define poll-supers-auto
  (schedule-recurring-task (lambda () (channel-put control 'supers)) (offset-hours->seconds 1 #:offset 60)))

;; Tower updater

(define (parse-tower-data lst)
  (filter-map (lambda (km) (if (positive? (sql-killmail-location km))
			       (let ([location (sql-parse->struct (parse-map (sql-killmail-location km))
								  #:struct mapDenormalize)])
				 (flatten
				  (list
				   (sql-killmail-region km)
				   (mapDenormalize-system location)
				   (cdr (split-moon-display (mapDenormalize-name location)))
				   (sql-killmail-corporationid km)
				   (sql-killmail-allianceid km)
				   (sql-killmail-datetime km)
				   (sql-killmail-shiptype km)
				   (sql-killmail-killid km))))
			       #f))
	      lst))

(define (towers-api-helper)
  (sql-tower-update-raw
   (parse-tower-data
    (zkill-poll-helper '("365") #:table "towerKillRaw" #:losses #t #:location #f #:moons #t))))

(define poll-towers-auto
  (schedule-recurring-task (lambda () (channel-put control 'towers)) (offset-hours->seconds 2 #:offset 60)))

;; Scheduler

(define tasks
  (thread (lambda () (run-tasks))))

;; Main

(define workers (map create-worker (range 2)))

(log-output "Eve Task Dispatcher initiated" #:id 'main)

;; (displayln (date->string (current-date) "~5"))

;; (for-each (lambda (void) (channel-put control 'exit)) workers)
;; (for-each thread-wait workers)

;; (schedule-stop-task)

;; Wait for tasks to end

(define main
  (sync (thread-dead-evt tasks)))
