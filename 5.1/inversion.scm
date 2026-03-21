;; Time-stamp: <2026-03-21 16:32:05 kim>

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
       (equal? (candidate  '(4 8 0 7 1 5 3 6 2)) '(2 3 6 4 0 2 2 1 0))))

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

;; TODO: Replace calls to length with a single call to length
;;       at the outset.

(define (merge-invs vs ws vs-len)
  (if (null? vs)
      ws
      (if (null? ws)
	  vs
	  (let* ([v (car vs)]
		 [vss (cdr vs)]
		 [w (car ws)]
		 [wss (cdr ws)]
		 [vn (car v)]
		 [v-count (cdr v)]
		 [wn (car w)]
		 [w-count (cdr w)])
	    (if (<= vn wn)
		(cons (cons vn v-count)
		      (merge-invs vss ws (- vs-len 1)))
		(cons (cons wn (+ w-count vs-len)) 
		      (merge-invs vs wss vs-len)))))))

;; For lists of odd length, the first half is longer.

(define (half-list vs)
  (letrec ([visit (lambda (sp fp b acc)
		   (if (null? fp)
		       (cons (reverse acc) sp)
		       (let ([f (car fp)]
			     [fpp (cdr fp)])
			 (if b
			     (visit sp fpp (not b) acc)
			     (let ([s (car sp)]
				   [spp (cdr sp)])
			       (visit spp fpp (not b) (cons s acc)))))))])
    (visit vs vs #f '())))
	
(define (perm-to-inv-merge vs)
  (letrec ([visit (lambda (ws ws-len)
		    (if (<= ws-len 1)
			ws
			(let* ([split (half-list ws)]
			       [front (car split)]
			       [back (cdr split)]
			       [half-len (div ws-len 2)])
			  (if (= ws-len (* 2 half-len))
			      (merge-invs (visit front half-len)
					  (visit back half-len)
					  half-len)
			      (merge-invs (visit front (+ 1 half-len))
					  (visit back half-len)
					  (+ 1 half-len))))))])
    (let* ([vs-len (length vs)]
	   [zipped (map (lambda (x) (cons x 0)) vs)]
	   [unzipped (map (lambda (x) (cdr x)) (visit zipped vs-len))])
      unzipped)))

(unless (time (test-perm-to-inv perm-to-inv-merge))
  (errorf 'perm-to-inv-merge "failed its unit tests."))

;; So now we have an algorithm which converts permutations
;; to inversion tables in O(nlogn).

;; Inverse of a permutation. Knuth's method is essentially
;; based on doing a zip (to bring us to two line notation)
;; and then sort the elements on the first line.

;; TODO: Modify to use merged iota and length.
;; TODO: Implement Fisher-Yates shuffle from section 3.4.2

(define (test-inverse-perm candidate)
  (and (equal? (candidate '()) '())
       (equal? (candidate '(1 2 0)) '(2 0 1))
       (equal? (candidate '(2 0 1)) '(1 2 0))
       (equal? (candidate '(0 1 2 3 4)) '(0 1 2 3 4))
       (equal? (candidate '(2 7 4 9 8 3 5 0 6 1)) '(7 9 0 5 2 6 8 1 4 3))
       (equal? (candidate '(7 9 0 5 2 6 8 1 4 3)) '(2 7 4 9 8 3 5 0 6 1))
       (equal? (candidate '(3 4 5 2 1 0)) '(5 4 3 0 1 2))
       (equal? (candidate '(5 4 3 0 1 2)) '(3 4 5 2 1 0))
       (equal? (candidate '(3 1 0 2 4)) '(2 1 3 0 4))
       (equal? (candidate '(2 1 3 0 4)) '(3 1 0 2 4))
       ;; This is the example in the text Knuth uses.
       (equal? (candidate '(4 8 0 7 1 5 3 6 2)) '(2 4 8 6 0 5 7 3 1))))

(define (inverse-perm perm)
  (let* ([zipped (map (lambda (x y) (cons x y)) perm (iota (length perm)))]
	 [sorted (sort (lambda (x y) (< (car x) (car y))) zipped)]
	 [unzipped (map (lambda (x) (cdr x)) sorted)])
    unzipped))

(unless (time (test-inverse-perm inverse-perm))
  (errorf 'inverse-perm "failed its unit tests."))
