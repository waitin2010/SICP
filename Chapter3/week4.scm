;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    week4b: Environments
;;;;;;;;Who:        waitin2010
;;;;;;;;Date:       2013-05-28
;;;;;;;;Description:

(define x 3)
(let ((y 3) (z 4))
  (set! x (+ y z)) x)

((lambda (y z) 
   (set! x (+ y z)) x)
 3 4)

;2
(define x 3)
(let ((x 3) (y x))
  (set! x (+ y x)) x)

;3
(define (foo x)
  (let ((y x) (z 3))
    (+ y z)))
(foo 3)

;4
(define (foo x)
  (let ((y x) (z 3))
    (let ((t (+ y z)))
      (+ t y z))))
(define foo
  (lambda (x) 
    ((lambda (y z)
       ((lambda (t)
	  (+ t y z)) (+ y z))) y z)) x 3)))
(foo 3)
