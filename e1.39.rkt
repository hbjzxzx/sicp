#lang racket

(require "common/common.rkt")
(require "e1.37.rkt")

(define (tan-cf x k)
	(define (n i) (if (= i 1) x (* -1.0 (square x))))
	(define (d i) (- (* 2 i) 1))	
	(cont-frac n d k)
)


(define (tan-cf-iter x k)
	(define (n i) (if (= i 1) x (* -1.0 (square x))))
	(define (d i) (- (* 2 i) 1))	
	(cont-frac-iter n d k)
)

(tan-cf 0.785 100)
(tan-cf-iter 0.785 100)