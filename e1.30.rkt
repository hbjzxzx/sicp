#lang racket

(require "common/sum.rkt")

(define (sum-iter term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))	
			)	
	)
	(iter a 0)
)

(sum-iter id 1 (lambda (x) (+ 1 x)) 100)
(sum id 1 (lambda (x) (+ 1 x)) 100)