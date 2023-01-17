#lang racket

(require "common.rkt")
(provide divides?
		 prime?
		 fast-prime?
		 find-divisor
		 fermat-test
		 expmod)

(define (divides? test-divisor n)
	(= (remainder n test-divisor)
		0))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
	      ((divides? test-divisor n) test-divisor)	
		  (else (find-divisor n (+ 1 test-divisor)))
	))


(define (prime? n)
	(if (= n 1) 
		false
		(= (find-divisor n 2) n)))


; basic theory is that
; a1 * a2 mod n == (a1 mod n) * (a2 mod n)
; a1 * a2 mod n == (a1 * (a2 mod n)) mod n
(define (expmod base exp m)
	(cond ((= exp 0) 1)
		  ((even? exp) (remainder (square (expmod base (/ exp 2) m))
		  				m))	
		  (else (remainder
		  			(* base (expmod base (- exp 1) m)) m)
				)))

(define (expmod-trival b n m)
	(remainder (exp b n) m))

; fermat test
(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 
				 (if (< n 4294967087) (random (- n 1)) (random 4294967087))
			)
	)
)

;fast prime-test use fermat test
(define (fast-prime? n t)
	(cond ((= t 0) true)
		  ((fermat-test n) (fast-prime? n (- t 1)))
		  (else false)))