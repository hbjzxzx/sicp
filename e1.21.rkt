#lang racket

(require "common/prime.rkt")

(define (smallest-divisor n) (find-divisor n 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)