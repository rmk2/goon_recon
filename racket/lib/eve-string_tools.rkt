#lang racket

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

;; Translate moon names (System ROMAN - Moon INT) into a simpler format (System INT-INT)

(define (simplify-moon-display str)
  (let ([lst (string-split str)])
    (string-append (first lst)
		   " "
		   (number->string (roman->int (second lst)))
		   (third lst)
		   (fifth lst))))
