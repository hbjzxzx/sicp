#lang racket

(require "common/common.rkt")
(require "common/sum.rkt")


(define (simpson-integration f a b n)
	(define h (/ (- b a) n))
	(define (add-2h x) (+ x (* 2 h)))
	(* 
		(+
			(* 4 (sum f (+ a h) add-2h (- b h)))
			(* 2 (sum f (+ a (* 2 h)) add-2h (- b (* 2 h))))
			(f a)	
			(f b)
			)	
		(/ h 3)
	)	
)

(define (cube x) (* x (square x)))

(intergral cube 0 1.0 0.01)
(intergral cube 0 1.0 0.001)
(simpson-integration cube 0 1.0 100)
(simpson-integration cube 0 1.0 1000)
(simpson-integration cube 0 1.0 10000)