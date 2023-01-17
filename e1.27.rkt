#lang racket

(require "common/prime.rkt")


(define (fermat-possible n)
	(define (fermat-test x) 
		(= (expmod x n n) x))
	(define (fermat-iter i n acc)
		;;; (printf "fermat-iter ~a ~a ~a\n" i n acc)
		(cond ((>= i n) acc)
			  ((fermat-test i) (fermat-iter (+ i 1) n (+ acc 1)))	
			  (else (fermat-iter (+ i 1) n (acc 1)))
		)
	)
	(/ (fermat-iter 2 n 0) (- n 2))
)

; Carmicheal numbers
; 561 1105 1729 2465 2821
(define (show x) 
	(printf "~a: ~a" x (if (prime? x) "prime" "not a prime"))
	(display "the fermat-test pass rate: ")
	(fermat-possible x)
)

(show 561)
(show 1105)
(show 1729)
(show 2465)
(show 2821)

(show 1009)
(show 1019)
(show 1223)
(show 2251)
(show 2633)
