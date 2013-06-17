;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    Accumulator
;;;;;;;;Who    :    waitin2010
;;;;;;;;Date   :    04-15-2013
;;;;;;;;Description:An accumulator is a procedure that is called 
;;;;;;;;            repeatedly with a single numeric argument
;;;;;;;;            and accumulates its arguments into a sum.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-accumulator sum)
  (lambda (addent)
    (begin (set! sum (+ sum addent))
	   sum)))

(define acc (make-accumulator 5))
(acc 10)
(acc 10)

