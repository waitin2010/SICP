(define (make-vect x y)
  (cons x y))
(define (xcor-vect vector)
  (car vector))
(define (ycor-vect vector)
  (cdr vector))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1)
		(xcor-vect vect2))
	     (+ (ycor-vect vect1)
		(ycor-vect vect2))))
(define (sub-vect vect1 vect2)
   (make-vect (+ (xcor-vect vect1)
		 (xcor-vect vect2))
	      (+ (ycor-vect vect1)
		(ycor-vect vect2))))

(define (scalar-vect k vect)
  (make-vect (* k (xcor-vect vect))
	     (* k (ycor-vect vect))))

(define vect1 (make-vect 1 1))
(define vect2 (make-vect 2 2))

vect1
vect2
(display "vect1+vect2=")
(add-vect vect1 vect2)
(newline)
(display "vect1 - vect2 =")
(sub-vect vect1 vect2)
(newline)
(display "k * vect1 = ")
(scalar-vect 3 vect1)
(newline)


(define (make-segment start end)
  (list start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))

(define segment1 (make-segment vect1 vect2))
segment1
(start-segment segment1)
(end-segment segment1)