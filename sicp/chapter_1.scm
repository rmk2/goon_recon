* Building Abstractions with procedures

** The Elements of Programming

*** Evaluating Combinations

(* (+ 2 (* 4 6)) (+ 3 5 7))

*** Compound Procedures

; (define (<name> <formal parameters>) <body>)

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

*** The Substitution Model for Procedure Application

; normal-order evaluation

(define (f a)
  (+ (* (+ a 1) (+ a 1)) (* (* a 2) (* a 2))))

*** Conditional Expressions and Predicates

; (cond (<p1> <e1>) (<p2> <e2>) ... (<pn> <en>))

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

; or

(define (abs x)
  (cond ((< x 0) (- x))
	(else x)))

; or

(define (abs x)
  (if (< x 0)
       (- x)
       x))

; ----

(define (>= x y) (or (> x y) (= x y)))

; or

(define (>= x y) (not (< x y)))

*** Exercises

; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3

(define (square-grt-2 x y z)
  (define (sum-of-squares a b) (+ (* a a) (* b b)))
  (cond
   ((and (<= x y) (<= x z)) (sum-of-squares y z))
   ((and (<= y x) (<= y z)) (sum-of-squares x z))
   (else (sum-of-squares x y))))

; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; is the same as

(define (a-plus-abs-b a b)
  (+ a (abs b)))

; Exercise 1.5

;; (define (p) (p))

;; (define (test x y)
;;   (if (= x 0)
;;       0
;;       y))

;; (test 0 (p))

; The function enters an infinite loop since y has the value (p) which is
; defined as (p), thus never terminating the loop /if/ applicative-order
; evaluation is used!

*** Example: Square Roots by Newton's Method

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.001)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

*** Cube roots

(define (cube x) (* x x x))

(define (cb-rt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cb-rt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root x)
  (cb-rt-iter 1.0 x))

(define (sqrt x)
  (define (square n) (* n n))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

*** Linear Recursion and Iteration

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* product counter)
	      (+ counter 1))))
  (iter 1 1))

