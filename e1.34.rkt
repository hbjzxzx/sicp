#lang racket

(require "common/common.rkt")

(define (f g) (g 2))
(f square)
(f f)