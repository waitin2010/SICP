;;;;Date: 2013-03-18
;;;;Who : waitin2010
(define (append-end listData element)
  (cond ((null? listData) (list element))
	((pair? listData) (append listData (list element)))
	(else (list listData element))))

(define listData 1)
(define listData2 '())
(define listData3 '(1 2))
(define element 1)

;;;;Test: if it work
;;;;Answer: (1 1)
;;;;        (1)
;;;;        (1 2 1)
(append-end listData element)
(append-end listData2 element)
(append-end listData3 element)

(define (combine seq1 seq2)
  (accumulate append nil 
	      (map (lambda (x) (map (lambda (y) (list x y)) seq2)) seq1)))
(define (combine2 seq3 seqtwo)
  (accumulate append nil
	      (map (lambda (x) (map (lambda (y) (append x (list y))) seq3)) seqtwo)))
(define (triples n)
  (combine2 (enumerate-interval 1 n) (combine (enumerate-interval 1 n) (enumerate-interval 1 n))))

(define (filter item)
  (cond ((null? item) nil)
	((not-order? (car item)) (filter (cdr item)))
	(else (cons (car item) (filter (cdr item))))))
(define (not-order? triple)
  (let ((a (car triple))
	(b (cadr triple))
	(c (caddr triple)))
    (if (and (< a b) (< b c))
	#f
	#t)))

(define (sum triple)
  (+ (car triple)
     (cadr triple)
     (caddr triple)))

(define (sum? s triple)
  (= s (sum triple)))

(define (sum-triples s item)
  (cond ((null? item) nil)
	((sum? s (car item)) (cons (car item) (sum-triples s (cdr item))))
	(else (sum-triples s (cdr item)))))
(define (sum-triple-application s n)
  (sum-triples s (filter (triples n))))

(sum-triple-application 6 6)
