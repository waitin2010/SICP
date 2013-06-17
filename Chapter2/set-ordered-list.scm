;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    Sets as ordered list
;;;;;;;;Who    :    waitin2010
;;;;;;;;Date   :    03-28-2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    element-of-set?
;;;;Argument:    x(element) set
;;;;Return  :    true if x is in the set
;;;;             false if x is not in the set

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define x '1)
(define set '(1 2 3 4 5))
(define set1 '(6 7 8 9))
(define empty-set '())

(element-of-set? x empty-set) ;x is not in the empty set
(element-of-set? x set)  ;x is in the set
(element-of-set? x set1) ;x is not in the set1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    adjoin-set
;;;;Argument:    x(element)
;;;;             set
;;;;Return  :    set, if x is in the set
;;;;             set' that contains the element x if x is not in
;;;;             the set

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set x empty-set)
(adjoin-set x set)
(adjoin-set x set1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    intersection-set
;;;;Argument:    set1, set2
;;;;Return  :    set whose element is in both set1 and set2

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 
		     (intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

(define set3 '(5 6 7 8 9))
(intersection-set empty-set empty-set)
(intersection-set empty-set set)
(intersection-set set empty-set)
(intersection-set set set1)
(intersection-set set set3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    union-set
;;;;Argument:    set1, set2
;;;;Return  :    set whose element is in either set1 or set2

(define (uinon-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2) 
		       (cons x1 (union-set (cdr set1) (cdr set2))))
		      ((< x1 x2)
		       (cons x1 (union-set (cdr set1) set2)))
		      ((> x1 x2)
		       (cons x2 (union-set set1 (cdr set2)))))))))

(union-set empty-set empty-set)
(union-set empty-set set)
(union-set set empty-set)
(union-set set set1)
(union-set set set3)