#lang racket

(require "e1.37.rkt")

(define (n _) 1.0)
(define (d x) 
	(let 
		(
			(r (remainder x 3))
			(q (quotient x 3))	
		)
		(if (= r 2) 
			(* 2 (+ q 1)) 
			1)		
	)
)

(cont-frac n d 100)
(cont-frac-iter n d 100)