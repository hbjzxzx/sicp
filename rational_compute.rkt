#lang racket

(require "common/common.rkt")
(require rackunit)

(define (gcd a b)
	(if (= b 0) 
		a 
		(gcd b (remainder a b)))
)

(define (make-rat n d) 
	(let 
		((g (gcd (abs n) (abs d))))

		(if 
			(or (and (> n 0) (> d 0))
				(and (< n 0) (< d 0)))
			(cons (/ (abs n) g) (/ (abs d) g)) 
			(cons (* -1.0 (/ (abs n) g)) (/ (abs d) g))
		)
	) 	
	)
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
	(printf "~a/~a" (car x) (cdr x))
	(newline)
)

(define (add-rat x y)
	(make-rat
		(+ (* (numer x) (denom y))
		   (* (numer y) (denom y)))	
		(* (denom x) (denom y))
	)
)

(define (sub-rat x y)
	(make-rat 
		(- (* (numer x) (denom y))
		   (* (numer y) (denom x))	
		)	
		(* (denom x) (denom y))
	)
)

(define (mul-rat x y)
	(make-rat 
		(* (numer x) (numer y))	
		(* (denom x) (denom y))
	)
)

(define (div-rat x y)
	(make-rat
		(* (numer x) (denom y))	
		(* (denom x) (numer y))
	)
)

(define (equal-rat? x y)
	(= (* (numer x) (denom y))
	   (* (numer y) (denom x)) 
	)
)


(define one-half (make-rat 1 2))
(define one-third (make-rat 3 9))
(print-rat one-half)
(print-rat one-third)
(print-rat (make-rat 1 -3))

(display "Exercise 2.2;\n")

(define (make-point x y)
	(cons x y)
)
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
	(printf "x: ~a, y: ~a\n" (x-point p) (y-point p))
)

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))
(define (midpoint-segment l)
	(let 
		((s (start-segment l))
		 (e (end-segment l))	
		)	

		(make-point 
			(/ 
				(+ (x-point s) (x-point e))	
				2
			)	

			(/
				(+ (y-point s) (y-point e))	
				2
			)
		)
	)
)

(define l1 (make-segment 
				(make-point 1 0)
				(make-point 2 0)	
				))
(print-point (midpoint-segment l1))

(display "Exercise 2.4;\n")	
(define (cons1 x y)
	(lambda (m) (m x y))
)
(define (car1 z)
	(z (lambda (x y) x))
)
(define (cdr1 z)
	(z (lambda (x y) y))
)

(check-equal? (car1 (cons1 2 3)) 2)
(check-equal? (cdr1 (cons1 2 3)) 3)
(display "Exercise 2.4 check pass;\n")	

(display "Exercise 2.5 start;\n")	
(define (cons2 x y)
	(* (exp 2 x)
	   (exp 3 y))
)
(define (car2 z)
	(define (iter cnt2 cnt3 n)
		(cond 
			((= n 1) cnt2)
			((= (remainder n 6) 0)  (iter (+ 1 cnt2) (+ 1 cnt3) (/ n 6)))
			((= (remainder n 3) 0) (iter cnt2 (+ 1 cnt3) (/ n 3)))
			((= (remainder n 2) 0) (iter (+ 1 cnt2) cnt3 (/ n 2)))
		)	
	)
	(iter 0 0 z)
)

(define (cdr2 z)
	(define (iter cnt2 cnt3 n)
		(cond 
			((= n 1) cnt3)
			((= (remainder n 6) 0)  (iter (+ 1 cnt2) (+ 1 cnt3) (/ n 6)))
			((= (remainder n 3) 0) (iter cnt2 (+ 1 cnt3) (/ n 3)))
			((= (remainder n 2) 0) (iter (+ 1 cnt2) cnt3 (/ n 2)))
		)	
	)
	(iter 0 0 z)
)
(check-equal? (car2 (cons2 2 3)) 2)
(check-equal? (cdr2 (cons2 2 3)) 3)
(check-equal? (car2 (cons2 800 10)) 800)
(check-equal? (cdr2 (cons2 800 10)) 10)
(display "Exercise 2.4 check pass;\n")	

