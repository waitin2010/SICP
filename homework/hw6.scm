;; Homework 6 (Due Monday, 8/2)
;; Name:
;; Login:

;;; Concurrency
(load "~cs61a/lib/concurrent.scm")

;; SICP:

;; EXERCISE 3.38
;: (set! balance (+ balance 10))
;: (set! balance (- balance 20))
;: (set! balance (- balance (/ balance 2)))


;; EXERCISE 3.39
;: (define x 10)
;: (define s (make-serializer))
;: (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;:                   (s (lambda () (set! x (+ x 1)))))


;; EXERCISE 3.40
;: (define x 10)
;: (parallel-execute (lambda () (set! x (* x x)))
;:                   (lambda () (set! x (* x x x))))
;: 
;: 
;: (define x 10)
;: (define s (make-serializer))
;: (parallel-execute (s (lambda () (set! x (* x x))))
;:                   (s (lambda () (set! x (* x x x)))))


;; EXERCISE 3.41

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance))))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;3.42 (optional)
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
	  (protected-deposit (protected deposit)))
      (define (dispatch m)
	(cond ((eq? m 'withdraw) protected-withdraw)
	      ((eq? m 'deposit) protected-deposit)
	      ((eq? m 'balance) balance)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m))))
      dispatch)))
      



;; EXERCISE 3.44 (optional)

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))


;3.46
; No Coding To Do here.

;3.48 (optional)

;; 2.2 Concurrent Stacks

;;;;;;;;;;;;;;;;;
;;; Streams
;;;;;;;;;;;;;;;;;

; 3.1 Smooth Stream


; 3.2 Mystery


; 3.3 Stream Scan


; SICP Exercises

; 3.50
(define (stream-map proc . argstreams)
  (if (<??> (car argstreams))
      the-empty-stream
      (<??>
       (apply proc (map <??> argstreams))
       (apply stream-map
              (cons proc (map <??> argstreams))))))
              
 ; 3.51
 
(define (show x)
  (display-line x)
  x)

;: (define x (stream-map show (stream-enumerate-interval 0 10)))
;: (stream-ref x 5)
;: (stream-ref x 7)


; 3.52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

;: (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;: (define y (stream-filter even? seq))
;: (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;:                          seq))

;: (stream-ref y 7)
;: (display-stream z)

 
; 3.53
;: (define s (cons-stream 1 (add-streams s s)))


; 3.54 Mul Streams (Optional)

; 3.55 Partial Sums (Optional)


; 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))