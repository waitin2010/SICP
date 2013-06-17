;;;;Date: 2013-03-21
;;;;Who: waitin2010
(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) true)
	((eq? (car list1) (car list2)) (equal? (cdr list1) (cdr list2)))
	(else false)))

(equal? '(this is a list) '(this is a list))
;Answer: #t
(equal? '(this (is a) list) '(this is a list))
;Answer: #f

(define (equal? list1 list2)
  (if (and (pair? list1) (pair? list2))
      (and (equal? (car list1) (car list2))
	   (equal? (cdr list1) (cdr list2)))
      (eq? list1 list2)))