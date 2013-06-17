;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;rand

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) 
	     (begin (set! x (rand-update x))
		    x))
	    ((eq? m 'reset)
	     (set! x random-init))
	    (else
	     (print "error"))))))

(define random-init 1)

(define (rand-n n)
  (if (= n 0)
      (rand 'generate)
      (cons (rand 'generate)
	    (rand-n (- n 1)))))

(rand-n 10)
(rand 'reset)
(rand-n 10)