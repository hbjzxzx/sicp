#lang racket

(require "common/prime.rkt")
(require "common/common.rkt")

(define (filtered-accumulate combiner filter null-value term a next b)
	(define (iter-acc acc a)
		(cond 
			((> a b) acc)
			((filter a) 
				(iter-acc 
					(combiner acc (term a)) 
					(next a)))
			(else (iter-acc acc (next a)))	
			)
	)
	(iter-acc null-value a)
)

(define (add x y) (+ x y))
(define (multi x y) (* x y))
(define (add1 x) (+ 1 x))
(define (id x) x)

(filtered-accumulate add (lambda (x) true)  0 id 0 add1 11)
(filtered-accumulate add prime?  0 id 0 add1 11)

(define (pproduct n)
	(define (filter x) (= 1 (gcd n x)))
	(filtered-accumulate multi filter 1 id 1 add1 n)
)
(pproduct 11)