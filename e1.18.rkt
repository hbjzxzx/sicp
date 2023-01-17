#lang racket

(require "common/common.rkt")

; both in log time and const space
(define (log_mul_opt a b)
	(define (log_mul_iter a b c)
		(cond 
			  ((= b 0) c)
			  ((even? b) (log_mul_iter (double a) (halve b) c))
			  (else (log_mul_iter (double a) (halve (- b 1)) (+ c a)))
		))
	(log_mul_iter a b 0)
)

(log_mul_opt 2 300)