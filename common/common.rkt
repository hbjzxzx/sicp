#lang racket

(provide even? 
		average 
		square
		double			
		halve
		exp
		not-equal?
				)

(define (average x y)
	(/ (+ x y) 2)
)

(define (even? n)
	(= (remainder n 2) 0))

(define (square x)
	(* x x))

(define (double n)
	(+ n n))

(define (halve n)
	(/ n 2))

(define (exp-iter base n acc) 
	(cond ((= n 0) acc)
		  ((even? n) (exp-iter (square base) (/ n 2) acc))
		  (else (exp-iter base (- n 1) (* acc base)))
	))

(define (exp b n)
	(exp-iter b n 1)
)

(define (not-equal? a b) 
	(not (= a b))
)

(define (gcd x y)
	(if (= y 0) x (gcd y (remainder x y)))
)
