#lang racket

(require "./common.rkt")

(define (improve guess x)
	(average guess (/ x guess))
)

(define (good-enough? guess new-guess)
	(< 
		(abs (- guess new-guess))
		0.00000000000000001
	)
)

(define (sqrt-iter guess x)
	(if (good-enough? guess (improve guess x))
		guess
		(sqrt-iter (improve guess x) x)
	)
)

(define (sqrt x)
	(sqrt-iter 1.0 x))

; this version will never end computeration.
; because the `new-if` procedure use an applicative order
; and the sqrt-iter-new-if's alternative will loop forever
(define (sqrt-iter-new-if guess x)
	(new-if (good-enough? guess x)
		guess
		(sqrt-iter-new-if (improve guess x) x)
	)
)

(define (new-if predicated then-clause else-clause)
	(cond (predicated then-clause)
		  (else else-clause))
)

(define (test x)
	(abs (- (square (sqrt x)) x))
)