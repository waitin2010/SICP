(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define test-reverse-sequence (list 1 2 3 4))

(reverse test-reverse-sequence)
(reverse2 test-reverse-sequence)