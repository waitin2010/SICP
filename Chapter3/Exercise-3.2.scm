;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    Monitor function calls
;;;;;;;;Who    :    waitin2010
;;;;;;;;Date   :    04-15-2013
;;;;;;;;Description: counting the numbers of function call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-monitored f)
  (let ((counter 0))
    (define (how-many-calls?)
      counter)
    (define (reset-count)
      (begin (set! counter 0)
	     counter))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
	    ((eq? m 'reset-count) (reset-count))
	    (else (begin 
		    (set! counter (+ 1 counter))
		    (f m)))))
    mf))

(define (square x) (* x x))
(define s (make-monitored square))
(s 10)
(s 'how-many-calls?)
