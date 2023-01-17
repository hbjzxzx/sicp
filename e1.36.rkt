#lang racket

(define tolerance 0.00001)
(define (average-damping f) (lambda (x) (/ (+ (f x) x) 2)))

(define (close-enough? a1 a2) (< (abs (- a1 a2)) tolerance ))

(define (fix-point f init-value) 
	(define (go-next v)
		(display v)
		(newline)	
		(fix-point f v)
	)
	(let (
			(next (f init-value))
		 )	
		(if (close-enough? init-value next) 
			next
			(go-next next)	
			)
			
	)
)

(define (f x) (/ (log 1000) (log x)))


(fix-point f 2)

(newline)
(display "avg damp")
(fix-point (average-damping f) 2)