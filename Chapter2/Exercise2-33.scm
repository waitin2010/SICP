(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define test-map (list 1 2 3 4))
(map square test-map)
;Answer: (1 4 9 16)
(map cube test-map)
;Answer: (1 8 27 64)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define test-append-seq1 (list 1 2 3 4))
(define test-append-seq2 (list 5 6 7 8))
(append test-append-seq2 test-append-seq1)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define test-length (list 1  2 3 4))
(length test-length)
;Answer: 4