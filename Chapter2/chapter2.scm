;2.2.3 Sequences as Conventional Interfaces 2013-03-14
;In this section, we introduce another powerful design principle for working with data structures--the use of conventional interfaces.

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares (car tree))
		 (sum-odd-squares (cdr tree)))))
;;;;;Constructs a list of all the even Fibonacci numbers Fib(k),where k is less
;;;;;than or equal to a given integer n
(define (even-fibs n)
  (define (next k)
    (if (> k n)
	nil
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (+ k 1)))
	      (next (+ k 1))))))
  (next 0))

;;;;;use the signal-flow diagrams
(define (square x) (* x x))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))
;Answer(1 3 5)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial 
      (op (car sequence)
	  (accumulate op initial (cdr sequence))))))
(accumulate + 0 (list 1 2 3 4 5))
;Answer: 15
(accumulate * 1 (list 1 2 3 4 5))
;Answer: 120
(accumulate cons nil (list 1 2 3 4 5))
;Answer: (1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)
;Answer: (2 3 4  5 6 7)

(define (sum-odd-squares tree)
  (accumulate + 
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons 
	      nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))
(list-fib-squares 10)
;Anwser:(0 1 1 4 9 25 64 169 441 1156 3025)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))
;Answer: 225

;;;;;Nested Mappings 2013-03-18;;;;;;;;;;;;
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? num)
  (define (iter factor num)
    (cond ((= factor num) #t)
       	  ((= (remainder num factor) 0) #f)
	  (else (iter (+ factor 1) num))))
  (iter 2 num))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		  (enumerate-interval 1 n)))))


;;;;;;;;;;;;;3. Symbal Data
(define a 1)
(define b 2)

(list a b)
;Answer: (1 2)
(list 'a 'b)
;Answer: (a b)
(list 'a b)
;Answer: (a 2)

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
;Answer: false
(memq 'apple '(x (apple sauce) y apple pear))
;Answer: (apple pear)

;;;;;;;symblo-differentition
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else
	 (error "unkown expression type--DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;;test for deriv function
(deriv '(+ x 3) 'x)
;Answer: (+ 1 0)
(deriv '(* x y) 'x)
;Answer: (+ (* x 0) (* 1 y))
(deriv '(* (* x y) (+ x 3)) 'x)
;Answer: (+ (* (* x y) (+ 1 0))
;           (* (+ (* x 0) (* 1 y))
;              (+ x 3)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))