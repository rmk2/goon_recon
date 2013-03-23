#! /usr/bin/guile  \
--use-srfi=27 -q -e main -s
!#

(use-modules (ice-9 format))

(define rsrc (make-random-source))
(random-source-randomize! rsrc)
(define random (random-source-make-integers rsrc))

(define (vref list n) (string->number (list-ref (cdr list) n)))
(define (roll n y) (format #t "(d~d~@d) ~d ~&" n y (+ 1 y (random n))))

(define (main args)
  (if (or (zero? (length (cdr args))) (> (length (cdr args)) 3))
      (format #t "Usage: ~a <type> [<amount>] [<modifier>] ~&" (car args))
      (let ((n (vref args 0)) (x 1) (y 0))
	(case (length (cdr args))
	  ((1) (let loop ((x x))
		 (if (positive? x) (begin (roll n y) (loop (- x 1))))))
	  ((2) (let loop ((x (vref args 1)))
	  	 (if (positive? x) (begin (roll n y) (loop (- x 1))))))
	  ((3) (let loop ((x (vref args 1)) (y (vref args 2)))
		 (if (positive? x) (begin (roll n y) (loop (- x 1) y)))))))))
