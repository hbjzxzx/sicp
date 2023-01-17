#lang racket
(define (f n)
  (cond ((< n 3) n)
         (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(f 20)

(define (fx n)
  (define (fxx a1 a2 a3 step)
    (cond ((= step 0) a1)
          (else (fxx (+ a1 (* 2 a2) (* 3 a3)) a1 a2 (- step 1)))))
  (cond ((< n 3) n)
        (else (fxx 2 1 0 (- n 2)))))

(fx 20)