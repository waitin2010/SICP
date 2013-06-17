;; Homework 2 (Due Tuesday, 7/6)
;; Name:
;; Login:

#|
The abstract data types, database, and procedures defined here will all be
used during the section on geopositioning. Before you begin the homework,
take a moment to familiarize yourself with the following definitions.
|#

;; POINT abstract data type
;; A point has two values: an x-coordinate and a y-coordinate, both of which
;; are numbers.
;;
;; Constructor: (make-point x y)
;; Selectors:   (get-x point)
;;              (get-y point)

(define make-point list)
(define get-x car)
(define get-y cadr)


;; RATED POINT abstract data type
;; A rated point has two values: a rating, which is a number, and a point.
;;
;; Constructor: (add-rating rating point)
;; Selectors:   (get-rating rated-point)
;;              (get-point rated-point)

(define add-rating list)
(define get-rating car)
(define get-point cadr)


;; CIRCLE abstract data type
;; A circle has two values: a radius, which is a number, and a center,
;; which is a point.
;;
;; Constructor: (make-circle radius center)
;; Selectors:   (radius circle)
;;              (center circle)

(define make-circle list)
(define radius car)
(define center cadr)


;; DATABASE
;; The database of MAC addresses and centers.
(define database
  (list
   (cons '48-2C-6A-1E-59-3D (make-point 0 0))
   (cons '2C-1E-54-A2-84-12 (make-point 8.36 8.41))
   (cons '82-1E-52-3B-DC-24 (make-point 1.13 6.47))
   (cons 'A3-C1-5D-73-12-9A (make-point 9.46 4.33))))


;; DIST
;; Takes two arguments, both points, and returns the distance between them.

(define (dist p1 p2)
  (let ((a (- (get-x p2) (get-x p1)))
	(b (- (get-y p2) (get-y p1))))
    (sqrt (+ (* a a) (* b b)))))


;; INSERTION-SORT-GENERAL
;; Takes two arguments, a comparator procedure and a list, and sorts the list
;; according to the comparator.

(define (insertion-sort-general comparator ls)
   (define (comp-insert num sorted comp)
     (if (null? sorted)
     (list num)
     (if (comp num (car sorted))
         (cons (car sorted) (comp-insert num (cdr sorted) comp))
         (cons num sorted))))

   (define (sort-help unsort sorted)
     (if (null? unsort)
     sorted
     (sort-help (cdr unsort)
        (comp-insert (car unsort) sorted comparator))))

   (sort-help ls '())
   
 )


;; FIRST-N-POINTS
;; Takes in a number and a list of points, and returns a new list of the
;; first n points in the original list.

(define (first-n-points n lop)
  (if (= n 0)
      '()
      (cons (car lop) (first-n-points (- n 1) (cdr lop)))))


;; INTERSECT
;; Takes in two circles, and returns the list of points at which they
;; intersect.

(define (intersect circle1 circle2)
  (let* ((a (get-x (center circle1)))
	 (b (get-y (center circle1)))
	 (c (get-x (center circle2)))
	 (d (get-y (center circle2)))
	 (r (radius circle1))
	 (s (radius circle2))
	 (e (- c a))
	 (f (- d b))
	 (p (sqrt (+ (* e e) (* f f)))))
    (if (or (< (+ r s) p) (= p 0) (< p (abs (- r s))))
	'()
	(let* ((k (/ (- (+ (* p p) (* r r)) (* s s)) (* 2 p)))
	       (x1 (+ a (/ (* e k) p)))
	       (x2 (/ (* f (sqrt (- (* r r) (* k k)))) p))
	       (y1 (+ b (/ (* f k) p)))
	       (y2 (/ (* e (sqrt (- (* r r) (* k k)))) p)))
	  (if (= (+ r s) p)
	      (list (make-point (+ x1 x2) (- y1 y2)))
	      (list (make-point (+ x1 x2) (- y1 y2))
				(make-point (- x1 x2) (+ y1 y2))))))))


;;; Homework starts here!
;; Fill in these procedures

;; Section 2: Big-Theta: What is the Runtime?
#|
Answers:
 1.
 2.
 3.
 4.
 5.
 6.
 7.
|#

;; Section 3: Lists
;; Question 1
(define (insert-front element ls)
  (error "Not Implemented!") ;; Your code goes here
  )

;; Question 2
(define (insert-back element ls)
  (error "Not Implemented!") ;; Your code goes here
  )

;; Section 4: Deep Lists
;; Question 1
(define (substitute ls old new)
 (error "Not Implemented!") ;; Your code goes here
 )
 
;; Question 2
(define (substitute2 ls old-ls new-ls)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Section 5: Trees
;; Question 1
(define (dfs-find tree pred)
 (error "Not Implemented!") ;; Your code goes here
 )
 
;; Question 3
(define (tree-plural tree)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Question 4
(define (tree-map tree proc)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Question 5
(define (has-path-sum? tree n)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Section 6: Binary Trees
;; Question 1
(define (tree-max tree)
 (error "Not Implemented!") ;; Your code goes here
 )
(define (tree-min tree)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Question 2
(define (tree-accumulate-preorder proc tree)
 (error "Not Implemented!") ;; Your code goes here
 )
(define (tree-accumulate-inorder proc tree)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Section 7: Case Study: Geopositioning via Wi-fi Signals
;; Question 1
(define (find-circle pair)
 (error "Not Implemented!") ;; Your code goes here
 )
 
;; Question 2
(define (intersection-points loc)
 (error "Not Implemented!") ;; Your code goes here
 )
 
;; Question 3
(define (distance-product point lop)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Question 4
(define (rate-points lop)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Question 5
(define (sort-points lorp)
 (error "Not Implemented!") ;; Your code goes here
 )
 
;; Question 6
(define (clumped-points lop)
 (error "Not Implemented!") ;; Your code goes here
 )

;; Question 7
(define (average-point lop)
 (error "Not Implemented!") ;; Your code goes here
 ) 

;; Question 8
(define (find-location lom)
 (error "Not Implemented!") ;; Your code goes here
 )

