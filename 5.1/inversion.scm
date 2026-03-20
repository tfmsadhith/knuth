;; Time-stamp: <2026-03-20 23:25:35 kim>

;; Run on Chez Scheme 10.2.0

;; Following along section 5.1.1 of Knuth

;; The input is an inversion table of the
;; form b1, b2, ..., bn, where we have
;; 0 <= b1 <= n - 1, 0 <= b2 <= n - 2, ...,
;; 0 <= bn <= 0.

;; The output is the corresponding
;; permutation.

(define (test-is-inversion-table? candidate)
  (and (equal? (candidate '())  #t)
       (equal? (candidate '(0)) #t)
       (equal? (candidate '(2 1 0)) #t)
       (equal? (candidate '(2 0 0)) #t)
       (equal? (candidate '(1 0 0)) #t)
       (equal? (candidate '(1 1 0)) #t)
       (equal? (candidate '(3 2 1 0)) #t)
       (equal? (candidate '(2 2 1 0)) #t)
       (equal? (candidate '(1 2 1 0)) #t)
       (equal? (candidate '(2 1 2)) #f)
       (equal? (candidate '(2 0 2)) #f)
       (equal? (candidate '(1 0 2)) #f)
       (equal? (candidate '(1 1 2)) #f)
       (equal? (candidate '(3 2 1 4)) #f)
       (equal? (candidate '(2 2 3 0)) #f)
       (equal? (candidate '(1 3 1 0)) #f)))

;; This is in O(n), but it is indispensable for
;; debugging purposes.

(define (is-inversion-table? table)
  (letrec ([visit (lambda (table n)
		    (if (null? table)
			#t
			(and (>= n (car table))
			     (>= (car table) 0)
			     (visit (cdr table) (- n 1)))))])
    (visit table (- (length table) 1))))

(unless (time (test-is-inversion-table? is-inversion-table?))
  (errorf 'is-inversion-table? "failed its unit tests."))

(define (test-inv-to-perm candidate)
  (and (equal? (candidate '()) '())
       (equal? (candidate '(0)) '(0))
       (equal? (candidate '(0 0)) '(0 1))
       (equal? (candidate '(0 0 0)) '(0 1 2))
       (equal? (candidate '(0 0 0 0)) '(0 1 2 3))
       (equal? (candidate '(0 0 0 0 0)) '(0 1 2 3 4))
       (equal? (candidate '(0 0 0 0 0 0)) '(0 1 2 3 4 5))
       (equal? (candidate '(0)) '(0))
       (equal? (candidate '(1 0)) '(1 0))
       (equal? (candidate '(2 1 0)) '(2 1 0))
       (equal? (candidate '(3 2 1 0)) '(3 2 1 0))
       (equal? (candidate '(4 3 2 1 0)) '(4 3 2 1 0))
       (equal? (candidate '(5 4 3 2 1 0)) '(5 4 3 2 1 0))
       (equal? (candidate '(4 2 2 1 0)) '(4 3 1 2 0))
       (equal? (candidate '(3 2 1 1 0)) '(4 2 1 0 3))
       (equal? (candidate '(7 7 2 2 0 0 1 1 0)) '(4 5 2 3 8 6 7 0 1))
       (equal? (candidate '(0 1 2 3 4 3 2 1 0)) '(0 8 1 7 2 6 3 5 4))
       ;; These are from exercise 5.1.1.1.
       (equal? (candidate '(5 0 1 2 1 2 0 0)) '(1 6 2 4 3 0 7 5))
       (equal? (candidate '(2 0 5 2 2 3 0 0 0)) '(1 6 0 7 3 4 8 2 5))
       ;; This is the example in the text Knuth uses.
       (equal? (candidate (list 2 3 6 4 0 2 2 1 0)) (list 4 8 0 7 1 5 3 6 2))))

;; TODO: Implement a tail-recursive version.
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

(unless (time (test-inv-to-perm inv-to-perm))
  (errorf 'inv-to-perm "failed its unit tests."))

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

(unless (time (test-inv-to-perm inv-to-perm-2))
  (errorf 'inv-to-perm-2 "failed its unit tests."))

;; Fusing reverse and iota.

(define (count-down n)
  (if (< n 0)
      (errorf 'count-down "only takes in non-negative inputs")
      (letrec ([visit (lambda (i acc)
			(if (= n i)
			    acc
			    (visit (+ i 1) (cons i acc))))])
	(visit 0 '()))))

(define (inv-to-perm-3 table)
  (if (is-inversion-table? table)
      (fold-left (lambda (ih x i)
		   (insert ih i x))
		 '()
		 (reverse table)
		 (count-down (length table)))
      (errorf 'inv-to-perm-3 "that was not an inversion table.")))

(unless (time (test-inv-to-perm inv-to-perm-3))
  (errorf 'inv-to-perm-3 "failed its unit tests."))

;; All of the above algorithms are in O(n^2)
;; there exist O(nlogn) algorithms based on
;; binary search trees.

;; Next, we want to be able to go from permutations to
;; inversion-tables. Specifically, we consider permutations
;; of (iota n). (Knuth hasn't introduced multi-set permutations
;; yet!)

;; TODO: Compose the two functions to get the identity for more
;;       tests.

(define (test-perm-to-inv candidate)
  (and (equal? (candidate '()) '())
     (equal? (candidate '(0)) '(0))
     (equal? (candidate '(0 1)) '(0 0))
     (equal? (candidate '(0 1 2)) '(0 0 0))
     (equal? (candidate '(0 1 2 3)) '(0 0 0 0))
     (equal? (candidate '(0 1 2 3 4)) '(0 0 0 0 0))
     (equal? (candidate '(0 1 2 3 4 5)) '(0 0 0 0 0 0))
     (equal? (candidate '(0)) '(0))
     (equal? (candidate '(1 0)) '(1 0))
     (equal? (candidate '(2 1 0)) '(2 1 0))
     (equal? (candidate '(3 2 1 0)) '(3 2 1 0))
     (equal? (candidate '(4 3 2 1 0)) '(4 3 2 1 0))
     (equal? (candidate '(5 4 3 2 1 0)) '(5 4 3 2 1 0))
     (equal? (candidate '(4 3 1 2 0)) '(4 2 2 1 0))
     (equal? (candidate '(4 2 1 0 3)) '(3 2 1 1 0))
     (equal? (candidate '(4 5 2 3 8 6 7 0 1)) '(7 7 2 2 0 0 1 1 0))
     (equal? (candidate '(0 8 1 7 2 6 3 5 4)) '(0 1 2 3 4 3 2 1 0))
     ;; These are from exercise 5.1.1.1.
     (equal? (candidate '(1 6 2 4 3 0 7 5)) '(5 0 1 2 1 2 0 0))
     (equal? (candidate '(1 6 0 7 3 4 8 2 5)) '(2 0 5 2 2 3 0 0 0))
     ;; This is the example in the text Knuth uses.
     (equal? (candidate (list 4 8 0 7 1 5 3 6 2)) (list 2 3 6 4 0 2 2 1 0))))
  
;; A simple algorithm in O(n^2).

;; Scans a list until we find i and
;; counts how many elements were greater
;; than i.

(define (more-than-i i vs)
  (letrec ([visit (lambda (vs acc)
		    (if (null? vs)
			(errorf 'more-than-i "i was not in the list")
			(let ([v (car vs)]
			      [vss (cdr vs)])
			  (if (equal? v i)
			      acc
			      (if (> v i)
				  (visit vss (+ acc 1))
				  (visit vss acc))))))])
    (visit vs 0)))
	   
;; TODO: Fuse iota and length.

(define (perm-to-inv vs)
  (map (lambda (i) (more-than-i i vs)) (iota (length vs))))
  
(unless (time (test-perm-to-inv perm-to-inv))
  (errorf 'perm-to-inv "failed its unit tests."))

;; A better algorithm is based on modifying merge sort.
