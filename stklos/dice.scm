#!/usr/bin/env stklos-script

(require "srfi-27")

(define rsrc (make-random-source))
(random-source-randomize! rsrc)
(define random (random-source-make-integers rsrc))

;; The same functionality working only when called within stklos 

;; (define (dice)
;;   (let ((input (read-line)))
;;     (if (null? (cdr (string-split input)))
;; 					; then
;; 	(let ((n (string->number input)))
;; 	  (if (and (number? n) (positive? n))
;; 	      (print "\(d" n "\) " (+ 1 (random n)))
;; 	      (printerr "Input needs to be a natural number")))
;; 					; else
;; 	(let* ((input (string-split input)) (n (string->number (car input))) (x (string->number (cadr input))))
;; 	  (if (and (number? n) (positive? n) (number? x) (positive? x))
;; 	      (do ((t x (- t 1)))
;; 		  ((zero? t))
;; 		(print "\(d" n "\) " (+ 1 (random n))))
;; 	      (printerr "Input needs to be two natural numbers")))))
;;   (dice))

(define (main arg)
  (let ((input (cdr arg))) ; (n (car input)) (x (cadr input)))
    (if (null? (cdr input))
					; then
	(let ((n (string->number (car input))) (x 1))
	  (if (and (number? n) (positive? n))
	      (do ((t x (- t 1)))
		  ((zero? t))
		(print "\(d" n "\) " (+ 1 (random n))))
	      (printerr "Input needs to be a natural number"))
	  (exit 0))
					; else
	(let ((n (string->number (car input))) (x (string->number (cadr input))))
	  (if (and (number? n) (positive? n) (number? x) (positive? x))
	      (do ((t x (- t 1)))
		  ((zero? t))
		(print "\(d" n "\) " (+ 1 (random n))))
	      (printerr "Input needs to be two natural numbers"))
	  (exit 0))))
  (main))
