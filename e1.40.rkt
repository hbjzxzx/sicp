#lang racket


(require "common/fix_point.rkt")
(require "common/common.rkt")

(define (cubic a b c)
	(lambda (x) (+
					(* x (square x))
					(* a (square x))
					(* b x)
					c
				)
	)
)

(define (newtons-method f guess)
	(fix-point (average-damping (newton-transform f)) guess)
)

(newtons-method (cubic 1.0 1.0 1.0) 2.0)