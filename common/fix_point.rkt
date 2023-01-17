#lang racket

(require "common.rkt")
(provide fix-point average-damping newton-transform fix-point-of-transform)


(define (average-damping f) (lambda (x) (/ (+ (f x) x) 2)))

(define (fix-point f first-guess)
	(define tolerance 0.00001)
	(define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
	(define (try guess)
		;;; (display guess)
		(let (
				(next (f guess))
			 )
			 (
				if (close-enough? guess next) next (try next)
			 ) 
			 ))
	(try first-guess)
)

(define dx 0.00001)
(define (deriv g)
	(lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)
(define (newton-transform g)
	(lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (fix-point-of-transform g transform guess)
	(fix-point (transform g) guess)
)