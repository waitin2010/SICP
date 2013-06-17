(define (fringe x)
  (cond ((null? x) (list))
	((pair? x) (append (fringe (car x)) (fringe (cdr x))))
	(else (list x))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;Answer:(1 2 3 4)
(fringe (list x x))
;(1 2 3 4 1 2 3 4)

;The code above is obvious, but inefficient
;Here's O(n) solution:
(define (fringe2 l)
  (define (accum l res)
    (cond ((null? l) res)
	  ((not (pair? l)) (cons l res))
	  (else (accum (car l) (accum (cdr l) res)))))
  (accum l null))