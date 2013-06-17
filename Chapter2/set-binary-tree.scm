;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;project:    sets as binary tree
;;;;;;;;Who    :    waitin2010
;;;;;;;;Date   :    03-28-2013

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Construtor & Selector for tree

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define tree (make-tree 7 
			(make-tree 3
				   (make-tree 1 '() '())
				   (make-tree 5 '() '()))
			(make-tree 9
				   '()
				   (make-tree 11 '() '()))))
(entry tree)
(left-branch tree)
(right-branch tree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function: element-of-set? 
;;;;Argument: element set
;;;;Return  ; true if element is in the set
;;;;          false if element is not in the set

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set)) 
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define empty-set '())
(element-of-set? 1 empty-set)
(element-of-set? 1 tree)
(element-of-set? 6 tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function: adjoin-set
;;;;Argument: element 
;;;;          set 
;;;;Return  ; set ,if element is already in the set
;;;;          set' that contains the element, if element is not i
;;;;          /n the set

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set)) 
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

(adjoin-set 2 tree)
(adjoin-set 1 tree)
(adjoin-set 8 tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    tree->list-1
;;;;Argument:    tree
;;;;Return  :    list
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

(tree->list-1 tree)
(tree->list-2 tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    list->tree
;;;;Argument:    an ordered list
;;;;Return  :    a balanced binary tree

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

(define list1 (list 1 3 5 7 9 11))
(list->tree list1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    intersection-set
;;;;Argument:    set1, set2
;;;;Return  :    set whose element is in both set1 and set2

(define (intersection-set set1 set2)
  (let ((list-set1 (tree->list-1 set1))
	(list-set2 (tree->list-1 set2)))
    (let ((intersection-set (intersection-set-ordered-list list-set1
							   list-set2)))
      (list->tree intersection-set))))


(define (intersection-set-ordered-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 
		     (intersection-set-ordered-list (cdr set1) (cdr set2))))
	      ((< x1 x2)
	       (intersection-set-ordered-list (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set-ordered-list set1 (cdr set2)))))))

(define tree2 '(5 (3 (2 () ()) (4 () ())) (8 (7 () ()) (9 () ()))))
(intersection-set tree tree2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Function:    union-set
;;;;Argument:    set1, set2
;;;;Return  :    set whose element is in either set1 or set2

(define (union-set set1 set2)
  (let ((list-set1 (tree->list-1 set1))
	(list-set2 (tree->list-1 set2)))
    (let ((union-set (union-set-ordered-list list-set1
					     list-set2)))
      (list->tree union-set))))

(define (uion-set-ordered-list set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2) 
		       (cons x1 (union-set-ordered-list (cdr set1) (cdr set2))))
		      ((< x1 x2)
		       (cons x1 (union-set-ordered-list (cdr set1) set2)))
		      ((> x1 x2)
		       (cons x2 (union-set-ordered-list set1 (cdr set2)))))))))

(define tree2 '(5 (3 (2 () ()) (4 () ())) (8 (7 () ()) (9 () ()))))
(union-set tree tree2)
