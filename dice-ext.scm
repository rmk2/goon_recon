#!/usr/bin/env stklos-script

(require "srfi-27")

(define rsrc (make-random-source))
(random-source-randomize! rsrc)
(define random (random-source-make-integers rsrc))

(define (main arg)
  (let ((input (cdr arg)))
    (cond
     ((null? input)
      (begin
	(printerr "Usage: dice <type> [<amount>] [<modifier>]")))
     ((null? (cdr input))
      (let ((n (string->number (car input))))
	(if (and (number? n) (positive? n))
	    (print "\(d" n "\) " (+ 1 (random n)))
	    (printerr "Input needs to be a natural number"))))
     (else
      (let ((n (string->number (car input))) (x (string->number (cadr input))) (y 0))
	(unless (null? (cddr input))
		(set! y (string->number (caddr input))))
	(if (and (number? n) (positive? n) (number? x) (positive? x) (number? y))
	    (do ((t x (- t 1)))
		((zero? t))
	      (cond
	       ((zero? y)
		(print "\(d" n "\) " (+ 1 (random n))))
	       ((positive? y)
		(print "\(d" n "+" y "\) " (+ 1 y (random n))))
	       ((negative? y)
		(print "\(d" n y "\) " (+ 1 y (random n))))))
	    (if (null? (cddr input))
		(printerr "Input needs to be two natural numbers")
		(printerr "Input needs to be two natural numbers and an optional real number"))))))))
