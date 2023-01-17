#lang racket

(require "prime.rkt")
(require rackunit)

(check-equal? (find-divisor 12 2) 2)
(check-equal? (prime? 12) false)
(check-equal? (prime? 11) true)
(check-equal? (prime? 100) false)

;;; (check-equal? (expmod 2 2 10) 4)
(check-equal? (expmod 2 10 100) 24)

(check-equal? (fermat-test 11) true)

(check-equal? (expmod' 2 10 1000) 24)
(expmod 2608 2632 2633)
(expmod 2608 1316 2633)
(expmod 2608 658 2633)
(expmod 2608 329 2633)
(display "all done")
