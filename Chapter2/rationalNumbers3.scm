;Arithmetic Operations for Rational Number 
;waitin2010-2013.03.06-15:00
;verison 3: can handles both positive and negative arguments

;gcd function: return the greatest factor of the two int
(define (gcd n d)
  (cond ((= n d) n)
	((> n d) (gcd (- n d) d))
	(else (gcd (- d n) n))))

(define (signum x)
  (if (= 0 x)
      0
      (/ x (abs x))))
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d)))
	(s (signum d)))
    (cons (/ n (* g s))
	  (/ d (* g s)))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (denom y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))