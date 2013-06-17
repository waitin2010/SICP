;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    Debug for proj2
;;;;;;;;Who:        waitin2010
;;;;;;;;Date:       2013-06-13
;;;;;;;;Description:

(define (name obj) (ask obj 'name))
(define (inventory obj)
  (if (person? obj)
      (map name (ask obj 'possessions))
      (map name (ask obj 'things))))
(define (description obj)
  (if (thing? obj)
      '(it is a thing)
      (ask obj description)))

(define (whereis person)
  (name (ask person 'place)))

(define (owner things)
  (let ((possessor (ask things 'possessor)))
    (if (equal? possessor 'no-one)
	'no-one
	(name possessor))))

(define (interleave ls1 ls2)
  (cond ((null? ls1) ls2)
	((null? ls2) ls1)
	(else (cons (car ls1) (interleave ls2 (cdr ls1))))))
(define ls '(1 3 5 7 9))
(define ls2 '(2 4 6 8 10))
(interleave ls ls2)

(define (directions-and-neighbors place)
  (let ((directions (ask place 'exits))
	(neighbor (map name (ask place 'neighbors))))
    (interleave neighbor directions)))

(define (people-in place)
  (map name (ask place 'people)))
