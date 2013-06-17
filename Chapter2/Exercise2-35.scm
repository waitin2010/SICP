;Redefine count-leaves
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))
(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y))
	      0
	      (map (lambda (x) 1)
		   (enumerate-tree t))))

(define test-count-leaves (list 1 (list 2 (list 3 4))))
(define test-count-leaves-two (list (list 1 3) (list 3 4)))
(count-leaves test-count-leaves)
(count-leaves test-count-leaves-two)