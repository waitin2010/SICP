;; Homework 7 (Due Monday, 8/9)
;; Name:
;; Login:

#| ================ +
 |  LAZY EVALUATOR  |
 | ================ |#

;;; 1. SICP Exercises.

;;; (a) 4.25

;;; (b) 4.27

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-Eval input:
count
;;; L-Eval value:
<response>

;;; Explain the response here.

;;; L-Eval input:
w
;;; L-Eval value:
<response>

;;; Explain the response here.

;;; L-Eval input:
count
;;; L-Eval value:
<response>

;;; Explain the response here.

;;; (c) 4.28

;;; 2.
;;; (a) In applicative order:
;;;
;;; (b) In normal order:
;;;

;;; 3.
;;; (a) Which is more efficient: the metacircular evaluator or
;;;     the lazy evaluator?  Why?
;;;
;;; (b) Which is more efficient: the metacircular evaluator or
;;;     the lazy evaluator?  Why?
;;;
;;; (c) Which is more efficient: the metacircular evaluator or
;;;     the lazy evaluator?  Why?
;;;


#| ============================ +
 |  NONDETERMINISTIC EVALUATOR  |
 | ============================ |#

;;; 1. SICP 4.35

(define (an-integer-between lower upper)
  (error "Question 1 not yet defined"))

;;; 2. SICP 4.37

;;; 3. SICP 4.42

#| =================== +
 |  LOGIC PROGRAMMING  |
 | =================== |#

;;; 1. SICP 4.56

;;; 2. SICP 4.58

;;; 3. 'length' relation

;;; 4. 'max' relation

;;; 5. (Optional, but good practice) 'depth' relation
