 ;; Section 2: Recursion
 ;; Question 1
(define (squares nums)
  (define (square x) (* x x))
  (if (empty? nums)
      '()
      (se (square (first nums))
	  (squares (bf nums))))
 )
 
 ;; Question 2
(define (switch sent)
  (define (replace word)
    (cond ((equal? word 'I) 'you)
	  ((equal? word 'me) 'you)
	  ((equal? word 'you) 'me)
	  (else word)))
  (define (switch-rec sent)
    (if (empty? sent)
	'()
	(se (replace (first sent))
	    (switch-rec (bf sent)))))
  (if (equal? (first sent) 'You)
      (se 'I (switch-rec (bf sent)))
      (switch-rec sent))
 )
 
 ;; Question 3a
(define (first-streak strk)
  (cond ((empty? strk) 0)
	((empty? (bf strk)) 1)
	((equal? (first strk) (first (bf strk))) (+ 1 (first-streak (bf strk))))
	(else 1))
 )
 ;; Question 3b
(define (best-streak strk)
  
 )
 
