#! /usr/bin/env racket
#lang racket

(require eve)

(define api (json-api "https://public-crest.eveonline.com/sovereignty/campaigns/"))

(define-syntax json-filter
  (syntax-rules (:name :defender-raw :defender :defender-name :attackers :system
		       :system-name :constellation :time :score :type-raw :type
		       :constellation-id :href :region :items)
    ((_ :name f) (hash-ref f 'name))
    ((_ :defender-raw hash) (hash-ref hash 'defender))
    ((_ :defender hash) (hash-ref (json-filter :defender-raw hash) 'defender))
    ((_ :defender-name hash) (json-filter :name (json-filter :defender hash)))
    ((_ :attackers hash) (hash-ref hash 'attackers))
    ((_ :system hash) (hash-ref hash 'sourceSolarsystem))
    ((_ :system-name hash) (json-filter :name (json-filter :system hash)))
    ((_ :constellation hash) (hash-ref hash 'constellation))
    ((_ :constellation-id hash) (hash-ref (json-filter :constellation hash) 'id_str))
    ((_ :time hash) (hash-ref hash 'startTime))
    ((_ :type-raw hash) (hash-ref hash 'eventType))
    ((_ :type hash) (case (hash-ref hash 'eventType)
		      [(1) "TCU"]
		      [(2) "IHUB"]
		      [(3) "Station"]
		      [(4) "Freeport"]))
    ((_ :href hash) (hash-ref hash 'href))
    ((_ :region hash) (hash-ref hash 'region))
    ((_ :items hash) (hash-ref hash 'items))))

(define-syntax freeport?
  (syntax-rules ()
    ((_ hash) (if (= (json-filter :type-raw hash) 4) #t #f))))

(define-syntax result-filter
  (syntax-rules ()
    ((_ filter list) (for-each (lambda (x) (if (memf (lambda (y) (regexp-match filter y)) x) (writeln x) #f)) list))))

(define-syntax result-print
  (syntax-rules (:filter :csv :tab)
    ((_ :filter filter list) (for-each (lambda (x) (if (memf (lambda (y) (regexp-match filter y)) x) (writeln x) #f)) list))
    ((_ :csv list) (for-each (lambda (x) (displayln x)) (map (lambda (y) (string-join y ",")) list)))
    ((_ :tab list) (for-each (lambda (x) (displayln x)) (map (lambda (y) (string-join y "\t")) list)))
    ((_ list) (result-print :filter ".*" list))))

;; Query API data

(define (crest-query)
  (let ([query-data (hash-ref api 'items)])
    (map (lambda (x) (list
		      (if (freeport? x) "*free-for-all" (json-filter :name (json-filter :defender x)))
		      (json-filter :type x)
		      (json-filter :name (json-filter :system x))
		      (json-filter :name (json-filter :constellation x))
		      (parse-region :name (parse-constellation :region (string->number (json-filter :constellation-id x))))
		      (json-filter :time x)))
	 query-data)))

;; (result-filter ".*" (crest-query))

;; (result-print :filter ".*" (crest-query))

(let ([data (crest-query)])
  (if (null? data)
      (exit)
      (begin
	(timerboard-prepare-table)
	(timerboard-replace data))))

;;(result-print :csv (crest-query)))
