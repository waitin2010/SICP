;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    homework 3
;;;;;;;;Who:        waitin2010
;;;;;;;;Date:       2013-05-24
;;;;;;;;Description:


;interface from erea or perimeter 
(define (area shape)
  (shape 'area))
(define (perimeter shape)
  (shape 'perimeter))

;circle abstraction
(define pi 3.14)
(define (make-circle r)
  (define (dispatch m)
    (cond ((equal? m 'area) (* pi r r))
	  ((equal? m 'perimeter) (* 2 pi r))
	  (else (error "NO this operation on circle" m))))
  dispatch)
;square abstraction
(define (make-square r)
  (define (dispatch m)
    (cond ((equal? m 'area) (* r r))
	  ((equal? m 'perimeter) (* 4 r))
	  (else (error "NO this operation on square" m))))
  dispatch)
;equilateral triangle abstraction
(define (make-e-triangle r)
  (define (dispatch m)
    (cond ((equal? m 'area) (* r r (/ (sqrt 3) 4)))
	  ((equal? m 'perimeter) (* 3 r))
	  (else (error "NO this operation on e-triangle" m))))
  dispatch)

;test area or perimeter of circle or square
(define c1 (make-circle 2))
(define c2 (make-circle 3))
(define s1 (make-square 2))
(define s2 (make-square 3))
(define e1 (make-e-triangle 2))
(define e2 (make-e-triangle 3))

(area c1)
(area c2)
(area s1)
(area s2)
(area e1)
(area e2)

(perimeter c1)
(perimeter c2)
(perimeter s1)
(perimeter s2)

(perimeter e1)
(perimeter e2)
