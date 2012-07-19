; Jerry D. Smith: An Introduction to Scheme (Prentice-Hall: New Jersey, 1988)

(print "Scheme is totally awesome")

(let ((x 3) (y 4) (z 5))
  (+ x y z))

; Define a function that takes an additional parameter without using lambda

(define (double x) (* x 2))

; Define the function dice to roll a x-sided die

(define dice (lambda (x) (print "\(d" x "\) " (+ 1 (random-integer x)))))

(dice 10)

; Print a given expression given as a paramater to a function

(define race (lambda (x) (print x)))

(race 'elf)

; Convert Celsius to Fahrenheit
(define fahrenheit (lambda (C) (+ (* (/ 9 5) C) 32)))

(fahrenheit 40)

; Convert Fahrenheit to Celsius
(define celsius (lambda (F) (* (/ 5 9) (- F 32))))

(celsius 104)

; Define a function the square a number, then to calculate the fourth power

(define square (lambda (n) (* n n)))
(define fourth-power (lambda (n) (* (square n) (square n))))

(square 2)
(fourth-power 2)

; Lists

(define foo '(1 2 3 4))

(car (cdr (cdr foo)))

(list-ref foo 0)

; Conditional Execution

(let ((x 1) (y 2) (z 3))
  (if (< y x) 'true 'false))

(let ((x 4) (y 3) (z 3))
  (cond
   ((< x y) (print x " is smaller than " y))
   ((< y z) (print y " is smaller than " z))
   (else
    (print "Welp!"))))

(define foo
  (lambda (n)
    (case n
      ((0)
       (print n " equals zero"))
      ((-1)
       (print n " equals minus one"))
      ((1)
       (print n " equals plus one"))
      (else
       (print "Welp!")))))

(define (vowel? (char)
  (case char
    ((a e i o u)
     'true)
    ((w y)
     'maybe)
    (else
     'false))))

; Enter a score, display the associated grade

(define grade
  (lambda (n)
    (cond
     ((and (<= n 100) (>= n 90))
      "A")
     ((and (<= n 89) (>= n 80))
      "B")
     ((and (<= n 79) (>= n 70))
      "C")
     ((and (<= n 69) (>= n 60))
      "D")
     ((and (<= n 59) (>= n 0))
      "F")
     (else
      (print "Invalid score. Score must be between 0 and 100.")))))

(grade 101)

; Repetitive Execution

(define one-to-n
  (lambda (n)
    (do ((i 1 (+ i 1))
	 (sum 0 (+ sum i)))
	((> i n) sum))))

(one-to-n 10)

; Do *without* update (bad form)

(define count-to-ten-bad
  (lambda (n)
    (do ((x 1))
    ((> x n) (newline))
  (begin
    (display x)
    (display "\t")
    (set! x (+ 1 x))))))

(bar 10)

; Do *with* update (good form)
; Short form, no lambda (n), but instead (define (<name> n))

(define (count-to-ten n)
  (do ((x 0 (+ 1 x))) ; (do ((<var> (<update>))) ((<termination>) <expr>) (body)
      ((> x n) (newline))
    (begin
      (display x)
      (display #\space))))

(baz 10)

(define (count-from-ten)
  (do ((x 10 (- x 1)))
      ((zero? x) (newline))
    (begin
      (display x)
      (display #\space))))

(count-from-ten)

(define (list-length lst)
  (do ((len 0 (+ len 1))
       (reduced-list lst (cdr reduced-list)))
      ((null? reduced-list) len)))

; Simple Recursion

; Factorial Function

(define (ftl n)
  (cond
   ((zero? n) 1)
   (else (* n (ftl (- n 1))))))

(ftl 5)

; Tail-recursive factorial function

(define (fact n)
  (define (tr-fact result n)
    (if (zero? n)
	result
	(tr-fact (* result n) (- n 1))))
  (tr-fact 1 n))


(fact 0)

; Tail-recursive implementation of list length

(define (r-l-length lst)
  (define (r-length len reduced-lst)
    (if (null? reduced-lst)
	len
	(r-length (+ 1 len) (cdr reduced-lst))))
  (r-length 0 lst))

(r-l-length '(1 2 3 4 5 6 7 8 9 10))

; car-cdr Recursion

(define (list-sum lst)
  (cond
   ((null? lst)
    0)
   ((pair? (car lst))
    (+ (list-sum (car lst)) (list-sum (cdr lst))))
   (else
    (+ (car lst) (list-sum (cdr lst))))))

(list-sum '(2 2))

; i-launch

(define (i-launch n)
  (do ((count n (- count 1)))
      ((zero? count) (print "B-L-A-S-T-O-F-F"))
    (print count)))

(i-launch 3)

; Let forms

(define (rawr n)
    (let ((x 1) (y 2))
      (* n (+ x y))))

(rawr 3)

; Named let

(define (ftrl n)
  (let tr-fact ((result 1) (n n))
    (if (zero? n)
	result
	(tr-fact (* result n) (- n 1)))))

(ftrl 5)

; tri-area

(define (tri-area base height)
  (let ((one-half (/ 1 2)))
    (* base height one-half)))

(tri-area 4 5)

; Characters and Strings

(char->integer #\a)

(substring "blafoobarbla" 3 9)

; Lambda Forms and Optional Arguments

(define testy
  (lambda (arg . opt)
    (if (null? opt)
	(print "One argument has been given")
	(print "Two arguments have been given"))))

(testy 'asd 'asd)

; format

(let ((arg 0))
  (format "~d ~d" arg (+ 2 arg)))
