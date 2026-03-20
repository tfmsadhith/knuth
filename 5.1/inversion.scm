;; Time-stamp: <2026-03-20 12:42:46 kim>

;; Following along section 5.1.1 of Knuth

;; The input is an inversion table of the
;; form b1, b2, ..., bn, where we have
;; 0 <= b1 <= n - 1, 0 <= b2 <= n - 2, ...,
;; 0 <= bn <= 0.

;; The output is the corresponding
;; permutation.

;; This algorithm is in O(n), but it is
;; indispensable for debugging purposes.

(define (is-inversion-table? table)
  (letrec ([visit (lambda (table n)
		    (if (null? table)
			#t
			(and (>= n (car table))
			     (>= (car table) 0)
			     (visit (cdr table) (- n 1)))))])
    (visit table (- (length table) 1))))

(define (test-inv-to-perm candidate)
  (and (equal? (candidate '()) '())
       (equal? (candidate (list 0)) (list 0))
       (equal? (candidate (list 2 3 6 4 0 2 2 1 0)) (list 4 8 0 7 1 5 3 6 2))))

(define (insert vs i n)
  (letrec ([visit (lambda (vs n)
		    (if (zero? n)
			(cons i vs)
			(if (null? vs)
			    (errorf 'insert "attempted to insert out of the list.")
			    (let ([v (car vs)]
				  [vss (cdr vs)])
			      (cons v (visit vss (- n 1)))))))])
		  (visit vs n)))

(define (inv-to-perm table)
  (if (is-inversion-table? table)
      (fold-right (lambda (x i ih)
		    (insert ih i x))
		  '()
		  table
		  (iota (length table)))
      (errorf 'inv-to-perm "that was not an inversion table.")))

(time (test-inv-to-perm inv-to-perm))

;; Bird-Wadler duality, insert is left-permutative.
;; We'd prefer fold-left over fold-right to avoid
;; stack overflows.

;; The cost of using fold-left is the O(n) reverse.

(define (inv-to-perm-2 table)
  (if (is-inversion-table? table)
      (fold-left (lambda (ih x i)
		   (insert ih i x))
		 '()
		 (reverse table)
		 (reverse (iota (length table))))
      (errorf 'inv-to-perm-2 "that was not an inversion table.")))


(time (test-inv-to-perm inv-to-perm-2))

;; Fusing reverse and iota.

(define (count-down n)
  (if (< n 0)
      (errorf 'count-down "only takes in non-negative inputs")
      (if (zero? n)
	  '()
	  (cons (- n 1) (count-down (- n 1))))))

(define (inv-to-perm-3 table)
  (if (is-inversion-table? table)
      (fold-left (lambda (ih x i)
		   (insert ih i x))
		 '()
		 (reverse table)
		 (count-down (length table)))
      (errorf 'inv-to-perm-3 "that was not an inversion table.")))

(time (test-inv-to-perm inv-to-perm-3))

;; All of the above algorithms are in O(n^2)
;; there exist O(nlogn) algorithms based on
;; binary search trees.
