#lang racket


(define (accumulate combiner null-value term a next b)
	(define (acc-iter acc a)
		(if (> a b) 
			acc 
			(acc-iter 
				(combiner acc (term a)) 
				(next a))
		))
	(acc-iter null-value a)
)

(define (product term a next b) 
	(accumulate (lambda (x y) (* x y))
				1	
				term
				a
				next
				b)
)

(define (sum term a next b)
	(accumulate (lambda (x y) (+ x y))
			    0	
				term
				a
				next
				b)
)

(define (id x) x)
(sum id 1 (lambda (x) (+ 1 x)) 100)
(product id 1 (lambda (x) (+ 1 x)) 100)