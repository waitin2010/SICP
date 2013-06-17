(define (reverse item)
  (define (rev item new-item)
    (cond ((not (null? item)) (rev (cdr item) (cons (car item) new-item)))
	  (else new-item)))
  (rev item (list)))

(define (deep-reverse item)
  (reverse (map reverse item)))

(define (deep-reverse-rec l)
  (if (not (pair? l))
      l
      (append (deep-reverse-rec (cdr l)) (list (deep-reverse-rec (car l))))))