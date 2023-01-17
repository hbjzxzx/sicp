#lang racket

(provide cont-frac
		 cont-frac-iter
)

(define (cont-frac n d k)
	(define (cal i) 
		(if (= i k) 
			(/ (n k) (d k))
			(/ (n i) (+ (d i) (cal (+ 1 i))))	
		)
	)
	(cal 1)
)


(define (filtered-accumulate combiner filter null-value term a next b)
	(define (iter-acc acc a)
		(cond 
			((> a b) acc)
			((filter a) 
				(iter-acc 
					(combiner acc (term a)) 
					(next a)))
			(else (iter-acc acc (next a)))	
			)
	)
	(iter-acc null-value a)
)

(define (cont-frac-iter n d k)
	(define (comb acc new-term) (new-term acc))
	(define (next i) (+ i 1))
	(define (term i) 
		(let 
			((index (- k i)))
			(lambda (v) (/ (n index) (+ (d index) v)))	
		))	
	(filtered-accumulate comb (lambda (x) true ) 0 term 0 next (- k 1))
)

(cont-frac
	(lambda (i) 1.0)
	(lambda (i) 1.0)
	100
)

(cont-frac-iter 
	(lambda (i) 1.0)
	(lambda (i) 1.0)
	100
)
