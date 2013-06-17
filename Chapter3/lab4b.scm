;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    Week 4B Lab
;;;;;;;;Who:        waitin2010
;;;;;;;;Date:       2013-05-24
;;;;;;;;Description:


(define (make-count init-amount)
  (let ((balance init-amount)
	(transaction '()))
    (define (withdraw amount)
      (set! transaction
	    (cons (cons 'withdraw amount)
		  transaction))
      (set! balance (- balance amount)) balance)
    (define (deposit amount)
      (set! transaction
	    (cons (cons 'deposit amount)
		  transaction))
      (set! balance (+ balance amount)) balance)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'balance) balance)
	    ((eq? m 'init-balance) init-amount)
	    ((eq? m 'transactions) transaction)))
    dispatch))

;test for make-count function
(define c1 (make-count 100))

((c1 'withdraw) 10)
((c1 'deposit) 10)
(c1 'balance)
(c1 'init-balance)
(c1 'transactions)


(define (plus1 var)
  (set! var (+ var 1))
  var)

(plus1 5)
;6

(define (make-adder n)
  (lambda (x) (+ x n)))

(make-adder 3)
;return function
((make-adder 3) 5)
;8

(define (f x) (make-adder 3))
(f 5)
;return function

(define g (make-adder 3))
(g 5) 
;8

(define (make-funny-adder n)
  (lambda (x)
    (if (equal? x 'new)
	(set! n (+ n 1))
	(+ x n))))
(define h (make-funny-adder 3))
;function h
(define j (make-funny-adder 7))
;function j
(h 5)
;8
(h 5)
;8
(h 'new)
;4
(h 5)
;9
(j 5)
;12

(let ((a 3))
  (+ 5 a))
;8

(let ((a 3))
  (lambda (x) (+ x a)))
;function

((let ((a 3))
   (lambda (x) (+ x a)))
 5)
;8

(define s 
  (let ((a 3))
    (lambda (msg)
      (cond ((equal? msg 'new)
	     (lambda ()
	       (set! a (+ a 1))))
	    ((equal? msg 'add)
	     (lambda (x) (+ x a)))
	    (else (error "hum?"))))))

(s 'add)
;f
(s 'add 5)
;error, two many arguments
((s 'add) 5)
;8
(s 'new)
;function
((s 'add) 5)
;8
((s 'new))
;okey
((s 'add) 5)
;9

((lambda (x)
   (let ((a 3))
     (+ x a)))
 5)
;8

(define k 
  (let ((a 3))
    (lambda (x) (+ x a))))

(k 5)
;8

(define m 
  (lambda (x)
    (let ((a 3))
      (+ x a))))
(m 5)
;8

(define p 
  (let ((a 3))
    (lambda (x)
      (if (equal? x 'new)
	  (set! a (+ a 1))
	  (+ x a)))))

(p 5)
;8
(p 5)
;8
(p 'new)
;okay
(p 5)
;9

(define r
  (lambda (x)
    (let ((a 3))
      (if (equal? x 'new)
	  (set! a (+ a 1))
	  (+ x a)))))

(r 5)
;8
(r 5)
;8
(r 'new)
;okay
(r 5)
;8

(define (ask obj msg .args)
  (apply (obj msg) args))

(ask s 'add 5)

(define answer 0)
(define (square  f x)
  (let ((answer 0))
    (f x) answer))
(square (lambda (n) (set! answer (* n n))) 3)
