;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;exercise:    set operations as unordered list represent
;;;;;;     Who:    Waitin2010
;;;;;;    Date:    2013-03-28
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function: element-of-set? 
;;;;Argument: element set
;;;;Return  ; true if element is in the set
;;;;          false if element is not in the set

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function: adjoin-set
;;;;Argument: element 
;;;;          set 
;;;;Return  ; set ,if element is already in the set
;;;;          set' that contains the element, if element is not i
;;;;          /n the set

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define set1 '(a b c d e f g))
(define x1 'a)
(define x2 'h)

(adjoin-set x1 set1)
(adjoin-set x2 set1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    intersection-set
;;;;Argument:    set1
;;;;             set2
;;;;Return  ;    set that contains the element is in the set1 and 
;;;;             meanwhile in the set2

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define set2 '())
(define set3 '(f g h i j k))
(define set4 '(h i j k))

(intersection-set set1 set2)
(intersection-set set1 set3)
(intersection-set set1 set4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:   union-set
;;;;Argument:   set1 
;;;;            set2 
;;;;Return  :   set that constains the element is either in 
;;;;            the set1 or set2 and both.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (adjoin-set (car set1) (union-set (cdr set1) set2)))))

(union-set set1 set2)
(union-set set1 set3)
(union-set set1 set4)
