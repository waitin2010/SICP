(define (split first-part second-part)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split first-part second-part) painter (- n 1))))
	  (first-part painer (second-part smaller smaller))))))