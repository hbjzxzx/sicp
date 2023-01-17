#lang racket

(require "common/common.rkt")

; mul tow number at space and time is N(b)
(define (linear_mul a b)
	(if (= b 0)
		0
		(+ a (linear_mul a (- b 1)))))

; mul two number at log time and log space
(define (log_mul a b)
	(cond ((= b 0) 0)
		  ((even? b) (log_mul (double a) (halve b)))
		  (else (+ (log_mul (double a) (halve (- b 1))) a))
		))



(linear_mul 2 300)
(log_mul 2 300)
