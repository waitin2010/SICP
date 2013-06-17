;Function: an alternative procedural representation of pairs
;Date: 2013-03-11 17:37
;who: waitin2010

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))