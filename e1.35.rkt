#lang racket

(require "common/fix_point.rkt")

(define (f1 x) (+ 1 (/ 1 x)))
(define (f2 x) (/ 2 x))
(define golden (fix-point (average-damping f1) 0.2))
(define sroot (fix-point (average-damping f2) 0.2))
golden