;1.Try these in Scheme:
(define x (cons 4 5))
(car x)
(cdr x)
(define y (cons 'hello 'goodbye))
(define z (cons x y))
(car (cdr z))
(cdr (cdr z))

;2.Predict the result of each these before you try it
(cdr (car z))
(car (cons 8 3))
(car z)
(car 3)

;3.Enter these definitions into Scheme
(define (make-rational num den)
  (cons num den))
(define (numerator rat)
  (car rat))
(define (denominator rat)
  (cdr rat))
(define (*rat a b)
  (make-rational (* (numerator a) (numerator b))
		 (* (denominator a) (denominator b))))
(define (print-rat rat)
  (word (numerator rat) '/ (denominator rat)))
(define (+rat a b)
  (make-rational (+ (numerator a) (numerator b))
		 (+ (denominator a) (denominator b))))
;4 Try this 
(print-rat (make-rational 2 3))
(print-rat (*rat (make-rational 2 3) (make-rational 1 4)))
(print-rat (+rat (make-rational 2 3) (make-rational 1 4)))

;7 lists
(define x '(a (b c) d))
(car x)
(cdr x)
(car (cdr x))