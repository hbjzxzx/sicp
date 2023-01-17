#lang racket

(require "common/common.rkt")

(define (remainder-check-notrival-square-root b n)
	(if (and 
			(not-equal? b 1)
			(not-equal? b (- n 1))	
			(= (remainder (square b) n) 1)
		)
		(error "find non-trival-square-root")
		(remainder (square (remainder b n)) n)
		)
)

; expmod Miller-Rabin
(define (expmod-mr b n m)
	(cond 
		  ((= n 0) 1) 
		  ((even? n) (remainder-check-notrival-square-root (expmod-mr b (/ n 2) m) m)) 
		  (else (remainder (* b (expmod-mr b (- n 1) m)) m))					
	)	
)

; fermat-test, Miller-Rabin
(define (fermat-test-mr n) 
	(define (try-it x n) 
		(= (expmod-mr x (- n 1) n) 1)
	)
	(try-it (+ 1 (random (- n 1))) n)
)

(define (fast-prime? x)
	(define (test-n-times x cnt)
		(cond 
			((= cnt 0) true) 
		    ((fermat-test-mr x) (test-n-times x (- cnt 1)))
			(else false)
		))
	(with-handlers  ([exn:fail? (lambda (_) false)])
				   (test-n-times x 20))
)


;;; (expmod-mr 2 10 100)

(fast-prime? 561)
(fast-prime? 1105)
(fast-prime? 1729)
(fast-prime? 2465)
(fast-prime? 2821)

(display "shou be true \n")
(fast-prime? 1009)
(fast-prime? 1019)
(fast-prime? 1223)
(fast-prime? 2251)
(fast-prime? 2633)