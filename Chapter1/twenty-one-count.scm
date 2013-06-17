(define (twenty-one-count lo-strategy mid-strategy hi-strategy lo-break-point hi-break-point count-scheme)
  (define deck (make-5-deck))
  (define seen-cards '())
  (define (dealer-loop dealer-hand customer-hand)
    (let ((dealer-total (best-total dealer-hand))
	  (custom-total (best-total customer-hand)))
    (cond ((> dealer-total 21) 1)
	  ((< dealer-total 17)
	   (begin (set! seen-cards (se (first deck) seen-cards))
		  (set! deck (bf deck))
		  (dealer-loop (se (first seen-cards) dealer-hand) customer-hand)))
	  ((< custom-total dealer-total) -1)
	  ((= custom-total dealer-total) 0)
	  (else 1))))
  (define (play customer-hand dealer-up-card)
    (let ((count (count-scheme seen-cards)))
    (cond ((> (best-total customer-hand) 21) -1)
	  ((or (and (<= count lo-break-point) (lo-strategy customer-hand dealer-up-card))
	       (and (> count lo-break-point) (< count hi-break-point) (mid-strategy customer-hand dealer-up-card))
	       (and (>= count hi-break-point) (hi-strategy customer-hand dealer-up-card)))
	   (begin (set! seen-cards (se (first deck) seen-cards))
		  (set! deck (bf deck))
		  (play (se (first seen-cards) customer-hand) dealer-up-card)))
	  (else
	   (set! seen-cards (se (first deck) seen-cards))
	   (set! deck (bf deck))
	   (dealer-loop (se (first seen-cards) dealer-up-card) customer-hand)))))

  (define (play-loop play-count)
    (let ((card1 (first deck))
	  (card2 (first (bf deck)))
	  (card3 (first (bf (bf deck)))))
      (if (= play-count 40) 0
	  (begin
	    (set! seen-cards (se seen-cards card1 card2 card3))
	    (set! deck (bf (bf (bf deck))))
	    (+ (play (se card1 card2) card3)
	       (play-loop (+ play-count 1)))))))
  
  (play-loop 0)
	        )

(define (play-n-count low mid high low-break high-break count-scheme n-executions)
  (if (<= n-executions 0) 0
      (+ (twenty-one-count low mid high low-break high-break count-scheme)
	 (play-n-count low mid high low-break high-break count-scheme (- n-executions 1)))))


;;NOTE: We have not learned vectors yet, so you do not need to understand how make-5-deck works.
(define (make-5-deck)
  (define deck-vector (vector 'Ah 'Ac 'As 'Ad '2h '2c '2s '2d '3h '3c '3s '3d '4h '4c '4s '4d '5h '5c '5s '5d '6h '6c '6s '6d '7h '7c '7s '7d '8h '8c '8s '8d '9h '9c '9s '9d '10h '10c '10s '10d 'jh 'jc 'js 'jd 'qh 'qc 'qs 'qd 'kh 'kc 'ks 'kd 'Ah 'Ac 'As 'Ad '2h '2c '2s '2d '3h '3c '3s '3d '4h '4c '4s '4d '5h '5c '5s '5d '6h '6c '6s '6d '7h '7c '7s '7d '8h '8c '8s '8d '9h '9c '9s '9d '10h '10c '10s '10d 'jh 'jc 'js 'jd 'qh 'qc 'qs 'qd 'kh 'kc 'ks 'kd 'Ah 'Ac 'As 'Ad '2h '2c '2s '2d '3h '3c '3s '3d '4h '4c '4s '4d '5h '5c '5s '5d '6h '6c '6s '6d '7h '7c '7s '7d '8h '8c '8s '8d '9h '9c '9s '9d '10h '10c '10s '10d 'jh 'jc 'js 'jd 'qh 'qc 'qs 'qd 'kh 'kc 'ks 'kd 'Ah 'Ac 'As 'Ad '2h '2c '2s '2d '3h '3c '3s '3d '4h '4c '4s '4d '5h '5c '5s '5d '6h '6c '6s '6d '7h '7c '7s '7d '8h '8c '8s '8d '9h '9c '9s '9d '10h '10c '10s '10d 'jh 'jc 'js 'jd 'qh 'qc 'qs 'qd 'kh 'kc 'ks 'kd 'Ah 'Ac 'As 'Ad '2h '2c '2s '2d '3h '3c '3s '3d '4h '4c '4s '4d '5h '5c '5s '5d '6h '6c '6s '6d '7h '7c '7s '7d '8h '8c '8s '8d '9h '9c '9s '9d '10h '10c '10s '10d 'jh 'jc 'js 'jd 'qh 'qc 'qs 'qd 'kh 'kc 'ks 'kd))  
  (define (shuffle index)
    (define (swap first-index swap-index)
      (if (= first-index swap-index)
	  (shuffle (+ index 1))
	  (begin
	    (let ((first-value (vector-ref deck-vector first-index))
		  (swap-value (vector-ref deck-vector swap-index)))
	    (vector-set! deck-vector first-index swap-value)
	    (vector-set! deck-vector swap-index first-value)
	    (shuffle (+ index 1))))))
    (if (= index 259)
	(vector->list deck-vector)
	(swap index (+ index (random (- 260 index))))))
  (shuffle 0))
