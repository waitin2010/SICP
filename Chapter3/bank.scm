;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    banking system
;;;;;;;;Who    :    waitin2010
;;;;;;;;Date   :    04-15-2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;First Version;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(withdraw 25)
(withdraw 25)
(withdraw 60)
(withdraw 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Second Version;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

(new-withdraw 25)
(new-withdraw 25)
(new-withdraw 60)
(new-withdraw 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Third version;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

(define W1 (make-withdraw 100))
(W1 25)
(W1 25)
(W1 60)
(W1 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Fourth Version;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	   balance))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unkown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100))

((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Fifth Version;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	   balance))
  (define (dispatch pwd m)
    (if (eq? pwd password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unkown request -- MAKE-ACCOUNT" m)))
	(error "Incorrect password" pwd)))
    
  dispatch)

(define acc (make-account 100 'secret))

((acc 'secret 'withdraw) 50)
((acc 'secret 'withdraw) 60)
((acc 'secret2 'deposit) 40)
((acc 'secret2 'withdraw) 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;sixth Version;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-account balance password)
  (let ((error-times 7))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! error-times 0)
	       (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! error-times 0)
	   (set! balance (+ balance amount))
	   balance))
  (define (call-the-cops)
    (print "You are stoling others' card"))
  (define (dispatch pwd m)
    (if (<= error-times 7)
	(if (eq? pwd password)
	    (cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unkown request -- MAKE-ACCOUNT" m)))
	    (begin (set! error-times (+ error-times 1))
		   (error "Incorrect password" pwd)))
	(call-the-cops)))
    
  dispatch))

(define acc (make-account 100 'secret))

((acc 'secret 'withdraw) 50)
((acc 'secret 'withdraw) 60)
((acc 'secret2 'deposit) 40)
((acc 'secret2 'withdraw) 60)
((acc 'secret2 'withdraw) 20)
((acc 'secret2 'deposit) 40)
((acc 'secret2 'withdraw) 60)
((acc 'secret 'withdraw) 2)
((acc 'secret2 'withdraw) 20)
((acc 'secret2 'deposit) 40)
((acc 'secret2 'withdraw) 60)
((acc 'secret2 'withdraw) 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Seventh Version;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	   balance))
  (define (dispatch pwd m)
    (if (eq? pwd password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unkown request -- MAKE-ACCOUNT" m)))
	(error "Incorrect password" pwd)))
    
  dispatch)

(define acc (make-account 100 'secret))

((acc 'secret 'withdraw) 50)
((acc 'secret 'withdraw) 60)
((acc 'secret2 'deposit) 40)
((acc 'secret2 'withdraw) 60)


(define (make-joint account password new-password)
  (lambda (pwd action)
    (cond ((eq? pwd new-password)
	   (account password action))
	  (else
	   (account new-password action)))))

(define new-acc (make-joint acc 'secret 'secret2))

((new-acc 'secret2 'withdraw) 50)
((new-acc 'secret2 'withdraw) 60)
((new-acc 'secret 'deposit) 40)
((new-acc 'secret2 'deposit) 40)