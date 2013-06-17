;Evaluating a polynomial in x at a given value of x by useing Horner's rule
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
		(+ (* higher-terms x) this-coeff))
	      0
	      coefficient-sequence))

(define test-horner-eval (list 1 3 0 5 0 1))
(horner-eval 2 (list 1 3 0 5 0 1))
;Answer:79