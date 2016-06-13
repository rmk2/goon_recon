#lang racket

(require srfi/19)

(provide (all-defined-out))

;; Convert a roman numeral into an integer

(define (roman->int string)
  (define (href hashtable string x)
    (hash-ref hashtable (string-ref string x)))
  (let ([ht (make-hash (map cons '(#\M #\D #\C #\L #\X #\V #\I) '(1000 500 100 50 10 5 1)))])
    (let loop ([string (string-upcase string)] [n 0] [x 0])
      (if (not (= (- (string-length string) 1) x))
	  (loop string
		((if (< (href ht string x) (href ht string (+ x 1))) - +) n (href ht string x))
		(+ x 1))
	  (+ n (href ht string x))))))

;; Translate moon names (System ROMAN - Moon INT) into a simpler format
;; (System Planet-Moon), outputting a string as result

(define (simplify-moon-display str)
  (cond [(regexp-match? #px"([A-Za-z0-9-]+\\s{0,1}\\w*)\\s([IVXL]+)\\s{0,1}[A-Za-z\\(\\)]*[ -]{3}\\w+\\s(\\d+)" str)
	 (let ([lst (regexp-match #px"([A-Za-z0-9-]+\\s{0,1}\\w*)\\s([IVXL]+)\\s{0,1}[A-Za-z\\(\\)]*[ -]{3}\\w+\\s(\\d+)" str)])
	   (format "~a ~a-~a" (second lst) (roman->int (third lst)) (fourth lst)))]
	[else str]))

;; Split moon names (System ROMAN - Moon INT) into a list (System Planet[INT]
;; Moon[INT]), which is useful if they are to end up in an sql table

(define (split-moon-display str)
  (cond [(regexp-match? #px"([A-Za-z0-9-]+\\s{0,1}\\w*)\\s([IVXL]+)\\s{0,1}[A-Za-z\\(\\)]*[ -]{3}\\w+\\s(\\d+)" str)
	 (let ([lst (regexp-match #px"([A-Za-z0-9-]+\\s{0,1}\\w*)\\s([IVXL]+)\\s{0,1}[A-Za-z\\(\\)]*[ -]{3}\\w+\\s(\\d+)" str)])
	   (list (second lst) (roman->int (third lst)) (string->number (fourth lst))))]))

;; Translate

(define-syntax iso8601->relaxed
  (syntax-rules ()
    ((_ str) (if (regexp-match #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}" str)
		 (date->string (string->date str "~Y-~m~dT~H:~M:~S") "~1 ~3")
		 str))))

;; Prettier form to check whether a string is empty

(define (string-empty? x) (equal? "" x))
