;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    week6a
;;;;;;;;Who:        waitin2010
;;;;;;;;Date:       2013-06-02
;;;;;;;;Description:

;;concurrency
(define x-protector (make-serializer))
(define protected-plus-10 (x-protector (lambda () (set! x (+ x 10)))))
(define protected-plus-20 (x-protector (lambda () (set! x (+ x 20)))))
(parallel-execute protected-plus-10 protected-plus-20)


;;Streaming Along
(define (ones) (cons 1 ones))

(define (integers-starting n)
  (cons n (lambda () (integers-starting (+ n 1)))))

;;Construct stream
(define  

;;construct streams through procedures
(define (list->stream ls)
  (if (null? ls)
      the-empty-stream
      (cons-stream (car ls) (list->stream (cdr ls)))))

(list->stream '(1 2 3 4 5 6 7 8 9))

(define (lists-starting n)
  (cons-stream (list n)
	       (stream-map (lambda (ls) (cons n ls)) (lists-starting (+ n 1)))))


(ss (list-starting 1))

(define (chocolate name)
  (define (helper n)
    (cons-stream name
		 (stream-append (really n) (helper (+ n 1)))))
  (define (really n)
    (cond ((= n 0) 
	   (cons-stream 'likes
			(cons-stream 'chocolate the-empty-stream))
	   (else (cons-stream 'really (really (- n 1)))))))
  (helper 1))

(ss (chocolate 'chung))
