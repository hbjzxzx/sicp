#lang racket

(require "common/common.rkt")
(require "common/fix_point.rkt")

(define (compose f g) (lambda (x) (f (g x))))
(define (inc x) (+ 1 x))

((compose square inc) 6)


; e1.43
(define (repeated f n) 
	(if (= n 0) 
		(lambda (x) x)
		(lambda (x) (f ((repeated f (- n 1)) x)))
	)
)

((repeated square 2) 5)


; e1.44
(define (smooth f) 
	(define dx 0.01)
	(lambda (x)
		(/ 
			(+ 
				(f (- x dx))	
				(f x)
				(f (+ x dx))
			)	
			3
		)
	)
)

(define (smooth-n-fold f n)
	((repeated smooth n) f)
)


;e1.45
(define (root-n x n)
	(define (avg-damping-times stime)
		(let ((max-support (- (exp 2 (+ stime 1)) 1)))
			(if (> max-support n) stime
				(avg-damping-times (+ stime 1))	
			)	
		)
	)
	(fix-point 
		((repeated average-damping (avg-damping-times 1)) 
			(lambda (y) (/ x (exp y (- n 1)))))
		1.0
	)
)


(root-n 2 100)