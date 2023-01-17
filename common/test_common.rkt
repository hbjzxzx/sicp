#lang racket

(require "common.rkt")
(require rackunit)

(check-equal? (exp 2 9) 512)
(check-equal? (exp 2 10) 1024)
(check-equal? (exp 2 11) 2048)
(check-equal? (exp 423 314321432141432) 1)

