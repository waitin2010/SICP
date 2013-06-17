;;Name:
;;Login:
;;hw4

;;; Scheme OOP
;;1
(define-class (random-generator range)
  (class-vars (acount 0))
  (initialize (set! acount (+ acount 1)))
  (method (number) (random range))
  (method (count) acount))

(define generator1 (instantiate random-generator 5))
(define generator2 (instantiate random-generator 100))
(define generator3 (instantiate random-generator 250))

(ask generator1 'number)
(ask generator2 'number)
(ask generator1 'count)




;;2
(define-class (calsodamachine cans)
  (method (buy nums)
	  (if (> nums cans)
	      "not enough cans"
	      (begin (set! cans (- cans nums)) "here you go")))
  (method (refill nums)
	  (set! cans (+ cans nums))))

(define calsodamachine1 (instantiate calsodamachine 10))
(define calsodamachine2 (instantiate calsodamachine 30))

(ask calsodamachine1 'buy 30)
(ask calsodamachine2 'buy 30)
(ask calsodamachine1 'refill 50)
(ask calsodamachine1 'buy 30)
(ask calsodamachine2 'buy 1)




;;3
(define-class (StanfordSodaMachine cans)
  (parent (calsodamachine cans))
  (method (shake)
	  (let ((result (ask self 'buy 1)))
	    (if (equal? result "here you go")
		"here you go"
		"go bears"))))

(define stanfordsodamachine (instantiate StanfordSodaMachine 10))
(ask stanfordsodamachine 'shake)
(ask stanfordsodamachine 'buy 10)
(ask stanfordsodamachine 'buy 9)
(ask stanfordsodamachine 'shake)




;;4
(define (helper ls index)
  (if (> index 0)
      (+ (car ls) (helper (cdr ls) (- index 1)))
      0))

(define ls '(1 2 3 4))
(helper ls 1)
(helper ls 3)
(define-class (ubiquitous counts)
  (instance-vars (index 0))
  (class-vars (count-ls '()))
  (method (simpleplus num)
	  (set! counts (+ counts num)))
  (method (allplus num)
	  (set! count-ls (cons num count-ls)))
  (method (count)
	  (let ((len (length count-ls)))
	    (if (= len index)
		counts
		(begin (set! counts (+ counts (helper count-ls (- len index))))
		       (set! index len)
		       counts)))))

(define counter1 (instantiate ubiquitous 5))
(define counter2 (instantiate ubiquitous 5))

(ask counter1 'simpleplus 3)
(ask counter1 'allplus 2)

(ask counter1 'count)
(ask counter2 'count)
	      





;;5
(define-class (button)
  (instance-vars (state true))
  (method (check) state)
  (method (switch) (set! state (not state))))

(define button1 (instantiate button))
(ask button1 'check)
(ask button1 'switch)
(ask button1 'check)

(define (find ls n)
  (cond ((null? ls) '())
	((= n 0) (car ls))
	(else (find (cdr ls) (- n 1)))))

(define ls '(1 2 3 4 5))
(find ls 3)
(find ls 4)
(define-class (machine)
  (instance-vars (button-ls '()))
  (method (add button)
	  (set! button-ls (cons button button-ls)))
  (method (remove)
	  (set! button-ls (cdr button-ls)))
  (method (check n)
	  (let ((button (find button-ls n)))
	    (if (null? button)
		"no such button"
		(ask button 'check))))
  (method (press n)
	  (let ((button (find button-ls n)))
	    (if (null? button)
		"no such button"
		(ask button 'press)))))

(define button2 (instantiate button))
(define machine1 (instantiate machine))
(ask machine1 'add button1)
(ask machine1 'add button2)

(ask machine1 'check 0)
(ask machine1 'check 1)
(ask machine1 'check 2)

(ask machine1 'remove)
(ask machine1 'check 1)

;;;;Below the Line
;;1
;;SICP exercise 3.3 goes here.  Modify the given make-account procedure.
(define (make-account balance pwd)
  (define (withdraw amount)
	(if (>= balance amount)
    	(begin (set! balance (- balance amount))
           	balance)
    	"Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define (dispatch m password)
    (if (equal? password pwd)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))
	(lambda (x) "error password")))
  dispatch)

(define counter1 (make-account 100 'hello))
(define counter2 (make-account 100 'counter2))

((counter1 'withdraw 'hello) 30)
((counter1 'deposit 'password) 30)


;;For SICP exercise 3.4, copy below your solution from 3.3, and then modify it from here.
(define (make-account balance pwd)
  (define error-pwd-count 7)
  (define (call-the-cops)
    "We think you are trying to steal someone acount,and we have called the police")
  (define (withdraw amount)
	(if (>= balance amount)
    	(begin (set! balance (- balance amount))
           	balance)
    	"Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define (dispatch m password)
    (if (equal? password pwd)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))
	(lambda (x) (if (> error-pwd-count 0)
			(begin (set! error-pwd-count (- error-pwd-count 1))
			       "error password")
			(call-the-cops)))))
  dispatch)

(define counter1 (make-account 100 'hello))

(define (help n pwd)
  (if (> n 0)
      (begin ((counter1 'withdraw pwd) 10)
      (help (- n 1) pwd))))

(help 8 'nice)




;;SICP exercise 3.8 goes here
(define x 2)
(define (f y)
  (if (= x 0)
      x
      (begin (set! x 0) y)))

(+ (f 0) (f 1))
(+ (f 1) (f 0))




;;2
;;Do not use define-class or OOP-Scheme in your re-definition of ubiquitous
(define (helper ls index)
  (if (> index 0)
      (+ (car ls) (helper (cdr ls) (- index 1)))
      0))

(define ls '(1 2 3 4))
(helper ls 1)
(helper ls 3)

(define ubiquitous
  (let ((count-ls '()))
    (lambda (counts)
      (let ((index 0))
	(define (simpleplus num)
	  (set! counts (+ counts num)))
	(define (allplus num)
	  (set! count-ls (cons num count-ls)))
	(define (count)
	  (let ((len (length count-ls)))
	    (if (= len index)
		counts
		(begin (set! counts (+ counts (helper count-ls (- len index))))
		       (set! index len)
		       counts))))
	(define (dispatch m)
	  (cond ((equal? m 'simpleplus) simpleplus)
		((equal? m 'allplus) allplus)
		((equal? m 'count) count)
		(else (error " No method in ubiquitous" m))))
	dispatch))))

(define counter1 (ubiquitous 3))
(define counter2 (ubiquitous 2))

((counter1 'simpleplus) 3)
((counter1 'allplus) 2)

((counter1 'count))
((counter2 'count))


;;;; Short Answer
#| Your answer goes here

first: 5, 6, 7
second: 5, 6, 7
Difference lis in: each first procedure have their's variable t while second procedure share one variable t.





|#
(define (foo x)
  (let ((t 1))
    (set! t (+ x 1))
    t))
(define foo
  (let ((t 1))
    (lambda (x) (set! t (+ x 1)) t)))
(foo 4)
(foo 5)
(foo 6)
;;; Section 5: Environment Diagrams
;;1 is an environment diagram question.  Bring the environment diagram with you to your face-to-face grading session.
(define x 3)
(define (foo z)
  (+ z x))
(foo 7)
(define (awesome x)
  (foo x))
(awesome 100)
;;2 is partially an environment diagram question.  Bring the environment diagram with you to your face-to-face grading session.
#|Explanation for 10 goes here:

4 will be return. Because we just create a procedure that will change x to 1, but we didn't call this procedure, so we will no change the variable x. So 4 will be return.



|#
(define x 4)
(define (changexto1 x)
  (set! x 1))
x
;;3 is an environment diagram.  Bring the environment diagram with you to your face-to-face grading session.
(define x 3)
(define (foo z)
  (+ z x))
(define (feed d)
  (foo d))
(define (amzaing h)
  (let ((x 4)
	(+ (lambda (s t) (+ s t x))))
    (feed (+ x h))))
(amazing 7)
;answer will be : 4 + 7 + 3 not 4 + 7 + 4.
;;; Secton 6: Extra for Experts
