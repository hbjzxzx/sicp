#lang racket

(define (iterative-improve good-enough? imporve)
	(define (iter x)
		(if (good-enough? x) x
			(iter (imporve x))	
			)
	)
	(iter 1.0)
)

(define (imporve-sqrt x)
	(lambda (guess)
		(/ 
			(+ 
				guess
				(/ x guess)	
			)
			2
		)
	)
)

(define (good-enough-sqrt x)
	(define tolerance 0.00001)
	(lambda (guess)
		(let ((next (imporve-sqrt x)))
			(if 
				(< 
					(abs (- (next guess) guess))
					tolerance
				)
				true
				false	
				)	
		)
	)
)

(define (sqrt x)
	(iterative-improve (good-enough-sqrt x) (imporve-sqrt x))
)

(sqrt 2)


