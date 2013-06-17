;;Date:2013-03-18
;;Who: waitin2010
(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define (extract s)
  (cond ((null? s) nil)
	(else (cons (caar s)
		    (extract (cdr s))))))
(extract seqs)
;Answer: (1 4 7)
(define (left s)
  (cond ((null? s) nil)
	(else (cons (cdar s)
		    (left (cdr s))))))
(left seqs)
;Answer: ((2 3) (5 6) (8 9))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (extract seqs))
	    (accumulate-n op init (left seqs)))))
;Answer:( 22 26 30)


;;;book Answer
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))