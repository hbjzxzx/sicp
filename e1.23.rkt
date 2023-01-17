#lang racket

(require "common/prime.rkt")
(require "common/common.rkt")

(define (good-next x) (if (= x 2) 3 (+ x 2)))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)	
          (else (find-divisor n (good-next test-divisor)))
    )
)


(define (prime? n)
    (= (find-divisor n 2) n))

(require racket/date)

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (current-inexact-milliseconds))
	)

(define (start-prime-test n start-time)
	(if (prime? n) 
		(report-prime (- (current-inexact-milliseconds) start-time))
		false		
		)
)

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time)
	true
)

(define (search-for-primes start next cnt)
	;;; (printf " start: ~a, cnt: ~a " start cnt)
	(define (print_and_next)
		(timed-prime-test start)	
		(search-for-primes (next start) next (- cnt 1))
		)
	(cond 
		((= cnt 0) "done")
		((prime? start) (print_and_next))
		;;; ((timed-prime-test start) (search-for-primes (next start) next (- cnt 1)))
		(else (search-for-primes (next start) next cnt))
	)
)

(define (next_odd x) 
	(if (even? x) (+ x 1) (+ x 2)))

(search-for-primes 1000 next_odd 3)
(search-for-primes 10000 next_odd 3)
(search-for-primes 1000000 next_odd 3)
(search-for-primes 100000000 next_odd 3)
(search-for-primes 10000000000 next_odd 3)
(search-for-primes 1000000000000 next_odd 3)
(search-for-primes 100000000000000 next_odd 3)
(search-for-primes 10000000000000000 next_odd 3)

;;; (timed-prime-tes