#! /usr/bin/env racket
#lang racket

(require eve)

;; Read from stdin

(define pipe-input (let ([input (second (read))])
		     (cond [(empty? input) (exit 0)]
			   [else (cdr (append* input))])))

;; Write to database

(define (sql-tower-create-raw)
  (if (table-exists? sqlc "towerKillRaw")
      #t
      (query-exec sqlc "CREATE TABLE towerKillRaw ( regionID INT NOT NULL, solarSystemID INT NOT NULL, planet INT NOT NULL, moon INT NOT NULL, corporationID INT NOT NULL, allianceID INT, datetime DATETIME NOT NULL, typeID INT, killID INT, UNIQUE KEY (solarSystemID, planet, moon) )")))

(define (sql-tower-update-raw lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO towerKillRaw VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE corporationID=?,allianceID=?,datetime=?,typeID=?,killID=?"
		     (first x)
		     (second x)
		     (third x)
		     (fourth x)
		     (fifth x)
		     (sixth x)
		     (seventh x)
		     (eighth x)
		     (ninth x)
		     (fifth x)
		     (sixth x)
		     (seventh x)
		     (eighth x)
		     (ninth x)))
	    lst))

;; Parse data into sql ready format

(define (parse-tower-data lst)
  (map (lambda (km) (let ([location (parse-map (list-ref km 7))])
		      (flatten
		       (list
			(list-ref km 9)
			(vector-ref location 3)
			(cdr (split-moon-display (vector-ref location 6)))
			(list-ref km 3)
			(list-ref km 5)
			(list-ref km 10)
			(list-ref km 0)
			(list-ref km 11)))))
       lst))

;; Exec

(sql-tower-update-raw (parse-tower-data pipe-input))
