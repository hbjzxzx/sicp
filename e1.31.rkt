#lang racket

(require "common/sum.rkt" "common/common.rkt")


(define (product term a next b)
	(if (> a b)
		1	
		(* (term a) (product term (next a) next b)))
)

(define (factorial n) (product id 1 (lambda (x) (+ 1 x)) n))

(define (sim-pi n)
	(define (t n) 
			(define cof (+ 1.0 (* 2 n)))
			(/ 
				(- (square cof) 1)
				(square cof)
			))
	(define (add n) (+ n 1))
	(* 4 (product t 1 add n))
)


(factorial 6)
(factorial 10)
(sim-pi 100)
(sim-pi 1000)
(sim-pi 1000000)