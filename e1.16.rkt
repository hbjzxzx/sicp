#lang racket

(require "common/common.rkt")

; define an invariant quantity that remains
; unchanged from state to state is a powerful way
(define (fast-expt b n)
	(define (iter-expt a b n)
		(cond ((= n 0) a)
			  ((even? n) (iter-expt a (* b b) (/ n 2)))
			  (else (iter-expt (* a b) (* b b) (/ (- n 1) 2)))
		))
			
	(iter-expt 1 b n))

(fast-expt 2 0)
(fast-expt 2 1)
(fast-expt 2 2)
(fast-expt 2 10)