#lang racket

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial	
		(op (car sequence)
			(accumulate op initial (cdr sequence))
		)
	)
)

(accumulate + 0 (list 1 2 3 4 5))

;e2.33
(define (map p sequence)
	(accumulate (lambda (x acc) (cons (p x) acc)) null sequence))

(define (append seq1 seq2)
	(accumulate cons seq2 seq1)
)

(define (length sequence)
	(accumulate (lambda (_ acc) (+ 1 acc)) 0 sequence)
)

(map (lambda (x) (* x x)) (list 1 2 3 4 5))
(append (list 1 2 3 4) (list 1 2 3 4))
(length (list 1 2 3 4))

;e2.34
(define (horner-eval x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms) 
					(+ (* higher-terms x) this-coeff)
					)
				0	
				coefficient-sequence
	)
)

(horner-eval 2 (list 1 3 0 5 0 1))

;e2.35
(define (count-leaves t)
	(accumulate 
		+
		0 
		(map (lambda (node) (if (pair? node) (count-leaves node) 1)) t)
	)
)
(define tree (list 1 2 (list 3 4) (list 5 (list 6 7)))) 
(count-leaves tree)

;e2.36
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		null
		(cons 
			(accumulate op init (map (lambda (seq) (car seq)) seqs))
			(accumulate-n op init (map (lambda (seq) (cdr seq)) seqs))
		)
	)
)
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 

;e2.37
(define (dot-product v w)
	(accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
	(map (lambda (row) (dot-product row m)) m)
)

(define (transpose mat)
	(accumulate-n cons null mat)
)

(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map 
			(lambda (row) 
				(map 
					(lambda (col) 
						(dot-product col row)) 
				cols)) 
			m)	
	)
)

(matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))) 