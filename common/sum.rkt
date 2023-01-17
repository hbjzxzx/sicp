#lang racket

(require "common.rkt")

(provide sum
		 intergral
		 id
)

(define (id x) x)

(define (sum term a next b)
	(if (> a b) 0 (+ 
					 (term a)
					 (sum term (next a) next b)))
)


(define (intergral f a b dx)
	(define (add-dx x) (+ x dx))
	(* 
		(sum f (+ a (/ dx 2)) add-dx b)	
		dx
	)
)

