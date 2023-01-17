#lang racket

(require rackunit)

(define (print-interval z)
	(printf "[~a, ~a]\n" (lower z) (upper z))
)

; e 2.7 start
(define (make-interval l u) (cons l u))
(define (lower i) (car i))
(define (upper i) (cdr i))
; e2.7 end

(define (add-interval x y)
	(make-interval 
		(+ (lower x) (lower y))	
		(+ (upper x) (upper y))
		)	
)

(define (mul-interval x y)
	(let 
		(
			(p1 (* (lower x) (lower y)))
			(p2 (* (lower x) (upper y)))
			(p3 (* (upper x) (lower y)))
			(p4 (* (upper x) (upper y)))
		)
		(make-interval
			(min p1 p2 p3 p4)
			(max p1 p2 p3 p4)
		)
	)
)

(define (div-interval-raw x y)
	(mul-interval
		x	
		(make-interval
			(/ 1.0 (upper y))	
			(/ 1.0 (lower y))
		)
	)
)

;e 2.8 start
(define (sub-interval x y) 
	(add-interval
		x	
		(make-interval
			(* -1.0 (upper y))	
			(* -1.0 (lower y))
		)
	)
)
;e 2.8 end

;e 2.10 start
(printf "e 2.10 start\n")
(define (div-interval x y)
	(mul-interval
		x	
		(let 
			(
				(width (- (upper y) (lower y)))
				(ry (/ 1.0 (upper y)))	
			)
			(if (<= width 0) 
				(let
					((a (* (lower x) ry))
					 (b (* (upper x) ry))	
					)	
				(make-interval (min a b) (max a b)))
				(make-interval
				(/ 1.0 (upper y))	
				(/ 1.0 (lower y)))
			)
		)
	)
)
(define x (make-interval 1 2))
(define y (make-interval 2 3))
(print-interval (div-interval x y))
(printf "e 2.10 end\n")
;e 2.10 end

;e 2.11
(printf "e 2.11 start\n")
(define (mul-interval-opti x y)
	(define (revert-interval x)
		(make-interval
			(* -1.0 (upper x))	
			(* -1.0 (lower x))
		)	
	)
	(define (mul-interval-pos-pos x y)
		(make-interval 
			(* (lower x) (lower y))
			(* (upper y) (upper y))
		)	
	)
	(define (mul-interval-pos-cross x y)
		(make-interval
			(* (upper x) (lower y))
			(* (upper x) (upper y))	
		)	
	)
	(define (mul-interval-pos-neg x y)
		(revert-interval (mul-interval-pos-pos x (revert-interval y)))
	)
	(define (mul-interval-cross-cross x y)
		(mul-interval x y)	
	)
	(define (mul-interval-cross-neg x y)
		(revert-interval (mul-interval-pos-cross (revert-interval y) x))	
	)
	(define (mul-interval-neg-neg x y)
		(make-interval
			(* (upper x) (upper y))	
			(* (lower x) (lower y))
		)
	)
	
	(define (neg-interval? x)
		(and 
			(< (lower x) 0)	
			(<= (upper x) 0)
		)	
	)
	(define (pos-interval? x)
		(and
			(>= (lower x) 0)	
			(> (upper x) 0)	
		)	
	)
	(define (cross-interval? x)
		(and
			(< (lower x) 0)	
			(> (upper x) 0)
		)	
	)
	(cond 
		((and (pos-interval? x) (pos-interval? y)) (mul-interval-pos-pos x y))	
		((and (pos-interval? x) (cross-interval? y) (mul-interval-pos-cross x y)))
		((and (pos-interval? x) (neg-interval? y)) (mul-interval-pos-neg x y))
		((and (cross-interval? x) (pos-interval? y)) (mul-interval-pos-cross y x))
		((and (cross-interval? x) (cross-interval? y) (mul-interval-cross-cross x y)))
		((and (cross-interval? x) (neg-interval? y) (mul-interval-cross-neg x y)))
		((and (neg-interval? x) (pos-interval? y) (mul-interval-pos-neg y x)))
		((and (neg-interval? x) (cross-interval? y) (mul-interval-cross-neg y x)))
		((and (neg-interval? x) (neg-interval? y)) (mul-interval-neg-neg x y))
		(else (error "not match any interval calculate"))
	)
)

(define (check-interval-equal? x y)
	(check-equal?
		(and
			(= (lower x) (lower y))	
			(= (upper x) (upper y))
		)
		true
	)
)

(printf "e 2.11 end\n")
(let 
	((x (make-interval 1 2))
	 (y (make-interval 1 2))	
	)
	(check-interval-equal? (mul-interval x y) (mul-interval-opti x y))
)

(let 
	((x (make-interval 1 2))
	 (y (make-interval -1 2))	
	)
	(check-interval-equal? (mul-interval x y) (mul-interval-opti x y))
)

(let 
	((x (make-interval 1 2))
	 (y (make-interval -2 -1))	
	)
	(check-interval-equal? (mul-interval x y) (mul-interval-opti x y))
)
;e 2.11 e


;e 2.12

(define (make-center-width c w)
	(make-interval (- c w) (+ c w))
)

(define (center i) 
	(/ 
		(+ 
			(lower i) 
			(upper i)
		)	
	2)
)

(define (width i)
	(/
		(-
			(upper i)
			(lower i)
		)	
	2)
)

(define (make-center-percent c p)
	(define (to_halve_float p) (/ p 200.0))
	(make-interval
		(- c (* c (to_halve_float p)))
		(+ c (* c (to_halve_float p)))
	)
)

(define (percent i)
	(let
		((c (center i))
		 (l (lower i))	
		)	
		(/ (- c l) c)
	)
)

(define l (make-center-percent 100 1))
(check-equal? (lower l) 99.5)
(check-equal? (upper l) 100.5)


; e 2.14
(define (par1 r1 r2)
	(div-interval
		(mul-interval r1 r2)
		(add-interval r1 r2)
	)
)

; par2 is correct
(define (par2 r1 r2)
	(let 
		((one (make-interval 1 1)))	
		(div-interval
			one (add-interval
					(div-interval one r1)	
					(div-interval one r2)
			)

		)
	)
)

(define r1 (make-center-percent 100 1))
(printf "r1 is: ")
(print-interval r1)
(define r2 (make-center-percent 70 1))
(printf "use par2")
(print-interval r2)

(printf "use par1")
(print-interval (par2 r1 r2))


(printf "r1/r1")
(print-interval (div-interval r1 r1))

(define A (make-center-percent 100.0 2))
(printf "A is: ")
(print-interval A)

(printf "A/A is")
(print-interval (div-interval A A))

(define B (make-center-percent 100.0 0))
(printf "B is: ")
(print-interval B)

(print-interval (div-interval A B))
