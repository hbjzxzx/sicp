#lang racket


(define (list-ref items n)
	(if (= n 0) 
		(car items)	
		(list-ref (cdr items) (- n 1))))


(define squares (list 1 4 9 16 25))
(list-ref squares 3)


(define (length items)
	(if (null? items)
		0
		(+ 1 (length (cdr items)))	
	)
)
(define odds (list 1 3 5 7 9))
(length odds)


(define (length-i items)
	(define (length-iter a count)
		(if (null? a) 
			count
			(length-iter (cdr a) (+ count 1))
		))
	(length-iter items 0)
)
(length-i odds)


(define (append list1 list2)
	(if (null? list1) 
		list2	
		(cons (car list1) (append (cdr list1) list2))))

(append odds squares)
(append squares odds)


; e2.17
(define (last-pair items)
	(if (null? (cdr items))
		(car items)	
		(last-pair (cdr items))
	)
)
(last-pair odds)
(last-pair squares)


(define (reverse as)
	(if (null? as)
		as
		(append (reverse (cdr as)) (list (car as)))
	)
)
(reverse odds)
(reverse squares)


(define (acc combiner init items)
	(if (null? items)
		init	
		(acc combiner (combiner init (car items)) (cdr items)))
	)
(acc (lambda (x y) (+ x y)) 0 odds)


(define (reverse-iter-style as)
	(define (cmb f next-item)
		(define (new-f) 
			(cons next-item (f))
			)
		new-f
	)
	(define (init) null)
	(let ((final (acc cmb init as)))
		(final)	
	)
)
(reverse-iter-style odds)
(reverse-iter-style squares)


;2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (no-more? x) (null? x))
(define (first x) (car x))
(define (except-first x) (cdr x))

(define (cc amount coins)
	(cond ((= amount 0) 1)
		  ((or (< amount 0) (no-more? coins)) 0)	
		  (else 
		  	(+ 
				(cc amount (except-first coins))	
				(cc (- amount (first coins)) coins)
			))
	))
(cc 100 (reverse us-coins))
(cc 100 us-coins)

(define (filter predicate items)
	(if (null? items) 
		null
		(let ( (first (car items))
				(remains (cdr items))
			)
			(if (predicate first)
				(cons first (filter predicate remains))	
				(filter predicate remains)
			)	
		)
	)
)

; e2.20
(define (same-parity . z)
	(define f (car z))
	(define (p x) (= (remainder x 2) (remainder f 2)))
	(cons
		f	
		(filter p (cdr z))
	)
)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)


(define (map proc items)
	(if (null? items)
		null	
		(cons (proc (car items))
			  (map proc (cdr items))	
		)
	)
)




;e2.21
(define (square-list items)
	(if (null? items)
		null
		(cons (* (car items) (car items)) (square-list (cdr items)))
	)
)
(square-list (list 1 2 3 4 5))
(define (square-list2 items)
	(map (lambda (x) (* x x)) items)
)
(square-list2 (list 1 2 3 4 5))

;e2.22 obvious, it's wrong

;e2.23
(define (for-each f items)
	(define (next items)
			(f (car items))
			(for-each f (cdr items))
	)
	(if (null? items)
		(newline)
		(next items)
	)
)
(for-each (lambda (x) 
			(newline)	
			(display x))
		(list 57 321 88))

;e2.25
(car (cdr (car (cddr (list 1 3 (list 5 7) 9)))))
(caar (list (list 7)))
(caddr (cddddr (list 1 2 3 4 5 6 7)))

;e2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;e2.27
(define (deep-reverse items)
	(if (null? items)
		null	
		(let (
			(first (car items))
			(remains (cdr items)))

			(if (pair? first)
				(append (deep-reverse remains) (list (deep-reverse first)))	
				(append (deep-reverse remains) (list first))
			)	
			)
	)
)
(deep-reverse (list (list 1 2) (list 3 4)))

;e2.28
(define (fringe items)
	(if (null? items)
		null	
		(let 
			((first (car items))
			 (remains (cdr items)))
			 
			(if (pair? first)
				(append (fringe first) (fringe remains))	
				(cons first (fringe remains))
			)
		)
	)
)
(define xx (list (list 1 2) (list 3 4)))
(fringe xx)
(fringe (list xx xx))

;e2.29
(define (make-mobile left right)
	(list left right)
)

(define (make-branch length structure)
	(list length structure)
)

(define (left-branch mobile)
	(car mobile)
)

(define (right-branch mobile)
	(cadr mobile)
)

(define (branch-length branch)
	(car branch)
)

(define (branch-structure branch)
	(cadr branch)
)

(define (total-weight mobile)
	(let 
		(
			(left (left-branch mobile))
			(right (right-branch mobile))
		)	

		(let (
			(left-struct (branch-structure left))
			(right-struct (branch-structure right)))
			
			(+
				(if (pair? left-struct) (total-weight left-struct) left-struct)	
				(if (pair? right-struct) (total-weight right-struct) right-struct)
			)
			)
	)
)

(define (balance-mobile? m)
	(define (branch-weight b)
		(let 
			(
				(struct (branch-structure b))
			)
			(if (pair? struct)
				(total-weight struct)	
				struct
			)
		)	
	)
	(let 
		(
			(left (left-branch m))
			(right (right-branch m))
		)
		
		(and
			(= 
				(* 
					(branch-length left) 
					(branch-weight left))
					
				(* 
					(branch-length right)
					(branch-weight right))	
			)
			
			(if (pair? (branch-structure left)) 
				(balance-mobile? (branch-structure left))
				true
			)
			
			(if (pair? (branch-structure right))
				(balance-mobile? (branch-structure right))
				true	
			)	
		)
	)
)

(define balance-m 
	(make-mobile
		(make-branch 10 (make-mobile
			(make-branch 1 50)
			(make-branch 1 50)))
		(make-branch 5 200)
		)
)
(total-weight balance-m)
(balance-mobile? balance-m)

(define unbalance-m
	(make-mobile
		(make-branch 1 20)
		(make-branch 1 
			(make-mobile
				(make-branch 10 10)	
				(make-branch 5 10)
			))	
	)
)

(total-weight unbalance-m)
(balance-mobile? unbalance-m)

;e2.30
(define (square-tree tree)
	(cond ((null? tree) null)
		  ((not (pair? tree)) (* tree tree))
		  (else (cons 
		 			(square-tree (car tree)) 
					(square-tree (cdr tree))
		  ))
	)
)
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7) 10))

(define (square-tree2 tree)
	(map (lambda (sub-tree) 
			(if (pair? sub-tree)
				(square-tree2 sub-tree)	
				(* sub-tree sub-tree)
			))
		tree	
		)
)
(square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7) 10))

;e2.31
(define (tree-map f tree)
	(map (lambda (sub-tree) 
			(if (pair? sub-tree)
				(tree-map f sub-tree)	
				(f sub-tree)
			))
		tree		
		)
)

(define (square-tree3 tree) (tree-map (lambda (x) (* x x)) tree))
(square-tree3 (list 1 (list 2 (list 3 4) 5) (list 6 7) 10))

;e2.32
(define (subsets s)
	(if (null? s)
		(list null)	
		(let (
				(rest (subsets (cdr s)))
			)
			
			(append rest (map (lambda (item) 
								(cons (car s) item)) rest))
		)	
				)	
)
(subsets (list 1 2 3))