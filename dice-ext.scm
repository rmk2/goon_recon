#!/usr/bin/env stklos-script

(require "srfi-27")

(define rsrc (make-random-source))
(random-source-randomize! rsrc)
(define random (random-source-make-integers rsrc))

(define-macro (vref n) `(string->number (list-ref input ,n)))

(define (main arg)
  (let ((input (argv)))
    (case (argc)
      ((1) (let ((n (vref 0)))
	     (if (and (number? n) (positive? n))
		 (print "\(d" n "\) " (+ 1 (random n)))
		 (printerr "Input needs to be a natural number"))))
      ((2) (let ((n (vref 0)) (x (vref 1)))
	     (if (and (number? n) (positive? n) (number? x) (positive? x))
		 (do ((t x (- t 1)))
		     ((zero? t))
		   (print "\(d" n "\) " (+ 1 (random n))))
		 (printerr "Input needs to be two natural numbers"))))
      ((3) (let ((n (vref 0)) (x (vref 1)) (y (vref 2)))
	     (if (and (number? n) (positive? n) (number? x) (positive? x) (number? y))
		 (do ((t x (- t 1)))
		     ((zero? t))
		   (cond
		    ((zero? y) (print "\(d" n "\) " (+ 1 (random n))))
		    ((positive? y) (print "\(d" n "+" y "\) " (+ 1 y (random n))))
		    ((negative? y) (print "\(d" n y "\) " (+ 1 y (random n))))))
		 (printerr "Input needs to be two natural numbers and an optional real number"))))
      (else (printerr "Usage: dice <type> [<amount>] [<modifier>]")))))
