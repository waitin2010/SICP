;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    Monte carlo simulation
;;;;;;;;who    :    waitin2010
;;;;;;;;Date   :    04-16-2013

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-int 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;approximation to pi by using monte carlo simulation

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) 
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(estimate-pi 10)
(estimate-pi 100)
(estimate-pi 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;estimating definite integrals

(define (random-in-range low hight)
  (let ((range (- hight low)))
    (+ low (random range))))

(define (area-rect x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (cesaro-test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (area-rect x1 x2 y1 y2) (monte-carlo trials cesaro-test)))

(define pi-predicate
  (lambda (x y)
    (<= (+ (* x x) (* y y)) 1)))

(estimate-integral pi-predicate -1 1 -1 1 10000)
