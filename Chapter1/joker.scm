(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )


;;;;best-total
(define (values-cards world)
  (let ((w (bl world)))
    (cond ((eq? w 'a) 'a)
	  ((or (eq? w 'j) (eq? w 'q) (eq? w 'k)) 10)
	  (else w))))
;;test for values-cards function
(map (lambda (x) (values-cards x)) (list 'as 'ad 'ac 'ah '1s '2d '3c '4h '10s 'js 'qs 'ks))

(define (best-total sent)
  (define (iter sent num-a sum-others)
    (if (empty? sent)
	(if (= num-a 0)
	    sum-others
	    (if (> (+ num-a 10 sum-others) 21)
		(+ num-a sum-others)
		(+ num-a 10 sum-others)))
	(let ((w (values-cards (first sent))))
	  (cond ((eq? w 'a) (iter (bf sent) (+ num-a 1) sum-others))
		(else (iter (bf sent) num-a (+ sum-others w)))))))
  (iter sent 0 0))

;;test for best-total function
(best-total '(ad 8s))
(best-total '(ad 8s 5h))
(best-total '(ad as 9h))

;;;;stop-at-15 stratege
(define (stop-at-15 customer-hands dealer-up-card)
  (< (best-total customer-hands) 15))

;;test for stop-at-15 strategy
(stop-at-15 '(ad 8s) '1s)
(stop-at-15 '(8s 5h) '1s)
(stop-at-15 '(8s 7s) '1s)

;;;;play-n function
(define (play-n strategy n)
  (if (= n 0)
      0
      (+ (twenty-one strategy)
	 (play-n strategy (- n 1)))))

;;test for play-n function
(play-n stop-at-15 10)
(play-n stop-at-15 1)

;;;;dealer-sensitive function
(define (dealer-sensitive customer-hands dealer-up-card)
  (or (and (or (eq? dealer-up-card 'a)
	      (eq? dealer-up-card '7)
	      (eq? dealer-up-card '8)
	      (eq? dealer-up-card '9)
	      (eq? dealer-up-card '10)
	      (eq? dealer-up-card 'j)
	      (eq? dealer-up-card 'q)
	      (eq? dealer-up-card 'k))
	      (< (best-total customer-hands) 17))
      (and (or (eq? dealer-up-card '2)
	   (eq? dealer-up-card '3)
	   (eq? dealer-up-card '4)
	   (eq? dealer-up-card '5)
	   (eq? dealer-up-card '6))
	   (< (best-total customer-hands) 12))))
;;test for dealer-sensitive function
(dealer-sensitive '(ah 6s) '8)
(dealer-sensitive '(ah 6s) '6)
(dealer-sensitive '(5s 5s) '2)
(dealer-sensitive '(5s 5s) '8)


;;;;stop-n function
(define (stop-at n)
 (lambda (customer-hands dealer-up-card)  (< (best-total customer-hands) n)))

;;test for stop-at-15 strategy
((stop-at 15) '(ad 8s) '1s)
((stop-at 15) '(8s 5h) '1s)
((stop-at 15) '(8s 7s) '1s)


;;;;earth function
(define (earth customer-hands dealer-up-card)
  (if (member? 's (map (lambda (x) (last x)) customer-hands))
      (stop-at 19)
      (stop-at 15)))