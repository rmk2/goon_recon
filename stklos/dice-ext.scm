#!/usr/bin/env stklos-script

(require "srfi-27")

(define rsrc (make-random-source))
(random-source-randomize! rsrc)
(define random (random-source-make-integers rsrc))

(define-macro (vref n) `(string->number (list-ref (argv) ,n)))
(define-macro (roll n y) `(if (negative? ,y)
			      (format #t "(d~d~d) ~d ~&" ,n ,y (+ 1 ,y (random ,n)))
			      (format #t "(d~d+~d) ~d ~&" ,n ,y (+ 1 ,y (random ,n)))))

(define (main arg)
  (if (or (zero? (argc)) (> (argc) 3))
      (printerr "Usage: dice <type> [<amount>] [<modifier>]")
      (let ((n (vref 0)) (x 1) (y 0))
	(case (argc)
	  ((1) (let loop ((x x))
		 (if (positive? x) (begin (roll n y) (loop (- x 1))))))
	  ((2) (let loop ((x (vref 1)))
		 (if (positive? x) (begin (roll n y) (loop (- x 1))))))
	  ((3) (let loop ((x (vref 1)) (y (vref 2)))
		 (if (positive? x) (begin (roll n y) (loop (- x 1) y)))))))))
