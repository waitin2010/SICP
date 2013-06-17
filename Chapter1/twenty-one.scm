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
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) '0j '0j) )

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
	  ((eq? w '0) '0)
	  ((or (eq? w 'j) (eq? w 'q) (eq? w 'k)) 10)
	  (else w))))
;;test for values-cards function
(map (lambda (x) (values-cards x)) (list 'as 'ad 'ac 'ah '1s '2d '3c '4h '10s 'js 'qs 'ks))

(define (type-card card)
  (last card))

(define (best-total-without-joker sent)
  (define (iter sent num-a  sum-others)
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

(define (count world sent)
  (cond ((empty? sent) 0)
	((eq? world (first sent)) (+ 1 (count world (bf sent))))
	(else (count world (bf sent)))))

;; test for count function
(count 'a '(a b c a d))
(count 'j '(a b c d j k j))
(count 'a (map (lambda (w) (first w)) '(as ds cs as ds)))
(define (best-total sent)
  (define (sum-others sent)
    (cond ((empty? sent) 0)
	  (else (if (eq? (values-cards (first sent)) 'a)
		    (sum-others (bf sent))
		    (+ (values-cards (first sent)) (sum-others (bf sent)))))))
  (let ((num-a (count 'a (map (lambda (w) (first w)) sent)))
	(num-j (count 'j (map (lambda (w) (last w)) sent)))
	(others (sum-others sent)))
    (cond ((= num-j 2) (if (> (+ others num-a 2) 21) (+ others num-a 2) 21))
	  ((= num-j 1) (cond ((= 0 num-a) 
			      (cond ((> (+ others 1) 21) (+ others 1))
				    ((< (+ others 11) 21) (+ others 11))
				    (else 21)))
			     (else (if (> (+ others num-a 11) 21)
				       (cond ((> (+ others num-a 1) 21) (+ others num-a 1))
					     ((< (+ others num-a 11) 21) (+ others num-a 11))
					     (else 21))
				       21))))
	  (else (best-total-without-joker sent)))))
  

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
      ((stop-at 19) customer-hands dealer-up-card)
      ((stop-at 17) customer-hands dealer-up-card)))

;;test for earth strategy
(earth '(ad 8s) '1d)
(earth '(5s 5s) '1d)
(earth '(ad 8s) '1s)
(earth '(5s 5s) '1d)


;;;;suit-strategy function
(define (suit-strategy suit strategy1 strategy2)
  (lambda (customer-hands dealer-up-card) 
    (if (member? suit (map (lambda (card) (type-card card)) customer-hands))
	(strategy1 customer-hands dealer-up-card)
	(strategy2 customer-hands dealer-up-card))))

(define (earth2 customer-hands dealer-up-card)
  ((suit-strategy 's (stop-at 19) (stop-at 17)) customer-hands dealer-up-card))

;;test for suit-strategy and earth2
(earth2 '(ad 8s) '1d)
(earth2 '(5s 5s) '1d)
(earth2 '(ad 8s) '1s)
(earth2 '(5s 5s) '1d)


;;;;majority function
(define (majority strategy1 strategy2 strategy3)
  (define (zero-one IsTrue)
    (if IsTrue 1 0))
  (lambda (customer-hands dealer-up-card)
    (let ((a (zero-one (strategy1 customer-hands dealer-up-card)))
	  (b (zero-one (strategy2 customer-hands dealer-up-card)))
	  (c (zero-one (strategy3 customer-hands dealer-up-card))))
    (< (+ a b c) 2))))

;;test for majority
((majority stop-at-15 dealer-sensitive earth2) '(5s 5s) '1s)