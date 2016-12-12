#lang racket

(require eve)
(require net/head)

;; Parameters

(define cl-delay (make-parameter 0))
(define cl-limit (make-parameter +inf.0))

;; Command-line argument handling

(define parse-args
  (command-line
   #:once-each
   [("-d" "--delay") n "Delay between iterations in seconds, default: 0"
    (let ([n (if (regexp-match? #px"^[[:digit:]]+$" n) (string->number n) n)])
      (cond [(and (number? n) (positive? n)) (cl-delay n)]
	    [else (raise-user-error "[error] Delay needs to be a positive number:" n)]))]
   [("-l" "--limit") n "Maximum number of iterations, default: +inf.0"
    (let ([n (if (regexp-match? #px"^(?:[[:digit:]]+|\\+inf\\.0)$" n) (string->number n) n)])
      (cond [(and (integer? n) (positive? n)) (cl-limit n)]
	    [(and (real? n) (infinite? n)) (cl-limit)]
	    [else (raise-user-error "[error] Limit needs to be a positive integer:" n)]))]))

;; Threading

(define input (make-channel))
(define control (make-channel))

(define (create-worker [id (gensym)])
  (thread (lambda () (let loop ([th (current-thread)])
		       (sync (wrap-evt input (lambda (hash) (sql-write-characters hash)))
			     (wrap-evt control (lambda (msg) (kill-thread th))))
		       (loop th)))))

;; Parse JSON

(define-syntax json-ref
  (syntax-rules (:href :id :id_str :name)
    ((_ hash key) (hash-ref hash key))
    ((_ :id hash key) (hash-ref (json-ref hash key) 'id))
    ((_ :id_str hash key) (hash-ref (json-ref hash key) 'id_str))
    ((_ :href hash key) (hash-ref (json-ref hash key) 'href))
    ((_ :name hash key) (hash-ref (json-ref hash key) 'name))))

;; Transform parsed JSON hashes into sql-character struct

(define (killmail->sql-character hash)
  (call/cc
   (lambda (return)
     (apply sql-character
	    (flatten
	     (list
	      (if (hash-has-key? hash 'character)
		  (list (json-ref :id hash 'character)
			(json-ref :name hash 'character))
		  (return null))
	      (json-ref :id hash 'corporation)
	      (json-ref :name hash 'corporation)
	      (if (hash-has-key? hash 'alliance)
		  (list (json-ref :id hash 'alliance)
			(json-ref :name hash 'alliance))
		  (list 0
			"NULL"))
	      (srfi-date->sql-timestamp (current-date))))))))

;; Write sql-character data for attackers and victims to SQL

(define (sql-write-characters hash)
  (sql-character-update-ids
   (filter sql-character? (map killmail->sql-character
			       (append (hash-ref hash 'attackers)
				       (list (hash-ref hash 'victim)))))))

;; Log killmail details to stdout (current-output-port)

(define (killmail->log hash)
  (printf "~a → ~a (killtime: ~a) → ~a~n"
	  (date->string (current-date) "~5")
	  (hash-ref hash 'killID)
	  (hash-ref hash 'killTime)
	  (json-ref :name (hash-ref hash 'victim) 'shipType)))

;; Connect to zkill's RedisQ endpoint, parse JSON response into hashes

(define (zkill-connect-redisq id)
  (read-json
   (get-pure-port (string->url "https://redisq.zkillboard.com/listen.php")
		  (list (format "Cookie: ~a" id)
			"Authority: redisq.zkillboard.com"
			"User-Agent: ryko@rmk2.org"))))

;; Loop through incoming responses, call log, extract character data

(define (zkill-poll-redisq id #:limit [limit +inf.0] #:delay [delay 0] #:workers [workers (map create-worker (range 2))])
  (call/cc
   (lambda (return)
     (let loop ([req (zkill-connect-redisq id)] [count 0])
       (cond [(and (not (infinite? limit))
		   (positive? limit)
		   (>= count limit))
	      (begin
		(for-each (lambda (void) (channel-put control 'exit)) workers)
		(return (display (format "Maximum number of iterations reached. Limit: ~a. Shutting down.~n" limit))))]
	     [(and (not (eof-object? req))
		   (not (equal? (hash-ref req 'package) (json-null))))
	      (let ([km (hash-ref (hash-ref req 'package) 'killmail)])
		(killmail->log km)
		(channel-put input km)
		(if (> delay 0) (sleep delay) (void))
		(loop (zkill-connect-redisq id) (+ count 1)))]
	     [else (loop (zkill-connect-redisq id) count)])))))

;; Exec

(let-values ([(body headers) (get-pure-port/headers (string->url "https://redisq.zkillboard.com/listen.php"))])
  (let ([id (car (string-split (extract-field "Set-Cookie" headers) "; "))])
    (zkill-poll-redisq id #:limit (cl-limit) #:delay (cl-delay))))
