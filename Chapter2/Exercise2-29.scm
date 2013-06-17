(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length structure)
  (car structure))
(define (branch-structure structure)
  (cadr structure))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
	(else (+ (branch-length (left-branch mobile))
		 (branch-length (right-branch mobile))
		 (total-weight (branch-structure (left-branch mobile)))
		 (total-weight (branch-structure (right-branch mobile)))))))

(define left (make-branch 1 nil))
(define right (make-branch 2 nil))
(define mobile (make-mobile left right))
(define mobile2 (make-mobile left (make-branch 3 mobile)))