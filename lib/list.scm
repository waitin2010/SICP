(define (last-pair l)
  (let ((Length (- (length l) 1)))
    (list (list-ref l Length))))

(define (last-pair2 l)
  (if (null? (cdr l))
      l
      (last-pair2 (cdr l))))


(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)
;the result:(10 20 30 40 50)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (map proc (cdr  items)))))

(map abs (list -10 2.5 -11.6 17))
;the result: ( 10 2.5 11.6 17)

(define (scale-lsit-new items factor)
  (map (lambda (x) (* x factor))
       items))