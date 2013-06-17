(define (unique-pairs n)
  (define (flatmap proc seq)
    (accumulate append nil (map proc seq)))
  (flatmap 
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
(unique-pairs 6)