(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k position))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))


(define empty-board nil)

(define (safe? k position) 
  (define (iter row k position)
    (cond ((or (null? position) (null? (cdr position))) #t)
	  ((or (= row (caar position)) (= (/ row k) (/ (caar position) (cadar position)))) #f)
	  (else (iter row k (cdr position)))))
  (cond ((null? position) #t)
	(else (iter (car (last position)) k position))))
    
(define test '((1 1) (3 2) (2 3)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

(define new-row 2)
(define k 4)
(adjoin-position new-row k test)