;Function: represent pairs of nonnegative integers using only numbers and arithmethic operation 
;Date: 2013-03-11 19:20
;Who:waitin2010

(define (square x) (* x x))
(define (fast-expt base index)
  (cond ((= index 0) 1)
	((= index 1) base)
	((= 0 (remainder index 2)) 
	 (square (fast-expt base (/ index 2))))
	(else (* base (fast-expt base (- index 1))))))
(define (fast-expt2 base index)
  (define (fast-expt2-iter base index result)
    (cond ((= index 0) result)
	  ((= index 1) (* base result))
	  ((= 0 (remainder index 2))
	   (square (fast-expt2-iter base (/ index 2) result)))
	  (else (* base (fast-expt2-iter base (- index 1)  result)))))
  (fast-expt2-iter base index 1))
(define (cons x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))

(define (factorize m base)
  (define (factorize-iter curr acc)
    (if (= 0 (remainder curr base))
	(factorize-iter (/ curr base) (+ acc 1))
	acc))
  (factorize-iter m 0))

(define (car z) (factorize z 2))
(define (cdr z) (factorize z 3))