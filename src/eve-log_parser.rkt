#!/usr/bin/env racket
#lang racket

(require eve)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;; Parameters

(define sensor (make-parameter "131"))
(define compmessenger (make-parameter "240"))

;; Database handling

(define sqlitec (sqlite3-connect #:database "/windows/Users/Ryko/Desktop/Eve/LogLite-2016-05-30_citadels.lsw" #:mode 'read-only))

(define (log-query-text channel) (query-rows sqlitec (string-append "SELECT message FROM messages WHERE channel = " channel)))

;; sensorSuite -> "OnSignalTrackerFullState"

(define ostfs-test
  (filter-map (lambda (lst)
		(findf (lambda (str) (regexp-match? #px"^OnSignalTrackerFullState" str)) lst))
	      (map vector->list (log-query-text (sensor)))))

(define (ostfs-split-text str) (let ([extract (regexp-match #px"(OnSignalTrackerFullState)\\s([0-9]{8})\\s(\\(.*\\))" str)])
				 (cdr extract)))

(define ostfs-lexer
  (lexer
   [(:= 8 numeric) (cons (list lexeme) (ostfs-lexer input-port))] ;; solarSystemID
   [(:+ (:: whitespace (:? "(") (:+ "{"))  (:: (:+ "}") (:? ")") punctuation)) (ostfs-lexer input-port)]
   [(:: (:? "-") (:+ numeric) (:? ".") (:? numeric)) (cons (string->number lexeme) (ostfs-lexer input-port))]
   [(:: (:+ alphabetic) (:? "-") (:+ numeric)) (cons lexeme (ostfs-lexer input-port))] ;; signature
   [(:or "L" punctuation whitespace) (ostfs-lexer input-port)]
   [(:+ alphabetic) (cons (list lexeme) (ostfs-lexer input-port))] ;; type
   [(eof) null]))

;; common.componentmessenger -> "OnAddedToSpace"

(define ccm-test
  (filter-map (lambda (lst)
		(findf (lambda (str) (regexp-match? #px"OnAddedToSpace" str)) lst))
	      (map vector->list (log-query-text (compmessenger)))))

(define (ccm-typeids->hash str)
  (let ([lst (string-split str "=")])
    (cons (string->symbol (car lst)) (string->number (cadr lst)))))

(define ccm-lexer
  (lexer
   [(:: (:+ alphabetic) (:? "ID") "=" (:+ numeric)) (cons (ccm-typeids->hash lexeme) (ccm-lexer input-port))] ;; IDs
   [(:or punctuation whitespace) (ccm-lexer input-port)]
   [(:or "L" (:+ alphabetic) (:+ numeric) "<" ">" "=" "[" "]") (ccm-lexer input-port)] ;; type
   [(eof) null]))

(define (ccm-string->hash str)
  (map (lambda (x) (make-hash (ccm-lexer (open-input-string x))))
       str))

;; Structures

(struct OnSignalTrackerFullState (type solarsystemid))

(struct anomaly (itemID typeID groupID categoryID x y z scanID) #:transparent)

(define (ostfs-list->struct lst [struct anomaly])
  (filter-map (lambda (anom) (cond [(not (empty? anom))
				    (call-with-values
					(lambda () (vector->values (list->vector anom)))
				      struct)]
				   [else #f]))
	      (split-list (memf (lambda (i) (and (number? i) (> i 1e12) (< i 1.09e12))) lst) 8)))

;; Cartesian math

(define (cartesian-distance origin target)
  (sqrt (apply + (map (lambda (i n) (expt (- i n) 2)) origin target))))

(define (parse-nearest-celestial #:solarSystemID id #:anomaly anom)
  (let ([anomaly-coord (list (anomaly-x anom) (anomaly-y anom) (anomaly-z anom))])
    (caar
     (sort
      (map (lambda (sql-coord) (cons (vector-ref sql-coord 0)
				     (cartesian-distance (take-right (vector->list sql-coord) 3) anomaly-coord)))
	   (query-rows sqlc "SELECT itemID,x,y,z FROM mapDenormalize_sde WHERE solarSystemID = ?"
		       id))
      <
      #:key (lambda (lst) (exact->inexact (cdr lst)))))))

;; Playground

;; (define test-anom (anomaly 1021222570614 35833 1657 65 -90587497223.0 -11095413640.0 -34670754303.0 "AHF-220"))

;; Laboratory

(define test-citadel (car (ostfs-list->struct (ostfs-lexer (open-input-string (third ostfs-test))))))
(define test-system (caadr (ostfs-lexer (open-input-string (third ostfs-test)))))

(let* ([citadel test-citadel]
       [system test-system]
       [citadel-data (car (filter (lambda (x) (equal? (anomaly-itemID test-citadel)
						      (hash-ref x 'itemID)))
				  (ccm-string->hash ccm-test)))])
  (list
   (anomaly-itemID citadel)
   (parse-nearest-celestial #:solarSystemID system #:anomaly citadel)
   (hash-ref citadel-data 'typeID)
   (hash-ref citadel-data 'corpID)
   (hash-ref citadel-data 'allianceID)
   (date->string (current-date) "~5")))
