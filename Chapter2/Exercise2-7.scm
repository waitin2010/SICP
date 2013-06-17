;Function: interval abstraction
;Date:2013-03-11 19:54
;Who:waitin2010

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((= 0 (upper-bound y)) (display "Error"))
	((= 0 (lower-bound y)) (display "Error"))
	(else (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))))

(define (mul-interval2 x y)
  (let ((a0 (lower-bound x))
	(b0 (upper-bound x))
	(a1 (lower-bound y))
	(b1 (upper-bound y)))
    (cond ((and (> a0 0) (> b0 0) (> a1 0) (> b1 0))
	   (make-interval (* a0 a1) (* b0 b1)))
	  ((and (> a0 0) (> b0 0) (< a1 0) (> b1 0))
	   (make-interval (* b0 a1) (* b0 b1))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-rationl c r)
  (make-interval (- c (* c r)) (+ c (* c r))))
(define  (ratio i)
  (/ (width i) (center i)))

;Exercise2.13
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval one 
				(add-interval (div-interval one r1)
					      (div-interval one r2))))))
;test1 3.5 0.15 test2  2 0.5
(define test1 (make-interval 3.5 0.15))
(define test2 (make-interval 2 0.5))