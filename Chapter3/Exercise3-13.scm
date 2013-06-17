;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    make cycle
;;;;;;;;Who:        waitin2010
;;;;;;;;Date:       2013-05-03
;;;;;;;;Description:

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x) 
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
z
;(c a b)

(last-pair z)
;cannot get an answer.
