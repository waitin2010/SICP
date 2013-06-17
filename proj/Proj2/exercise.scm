;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    proj2 
;;;;;;;;Who:        waitin2010
;;;;;;;;Date:       2013-06-06
;;;;;;;;Description:


(define dormitory (instantiate place 'dormitory))

(define waitin2010 (instantiate person 'waitin2010 dormitory))

(can-go dormitory 'south Soda)
(ask dormitory 'exits)

(define kirin (instantiate place 'kirin))
(can-go kirin 'north Soda) ;kirin lies in the north of soda

(define potstickers (instantiate thing 'potstickers))
(ask kirin 'appear potstickers)

(ask waitin2010 'go 'south)
;move to soda 
(ask waitin2010 'go 'north)
;move to kirin
(ask waitin2010 'take potstickers)
;take potstickers
;2D
(define computer (instantiate thing 'Durer))
(ask 61a-lab 'appear computer)
(ask 61a-lab 'appear Durer)
(ask 61a-lab 'appear 'Durer)

;2F
(define (name obj) (ask obj 'name))
(define (inventory obj)
  (if (person? obj)
      (map name (ask obj 'possessions))
      (map name (ask obj 'things))))
(name brian)
(name soda)
(inventory brian)
(inventory soda)

(define (whereis person)
  (ask (ask person 'place) 'name))
(whereis brian)

(define (owner things)
  (ask (ask things 'possessor) 'name))
(define p1 (instantiate person 'person1 soda))
(define t1 (instantiate thing 'things1))
(ask soda 'appear t1)
(ask p1 'take t1)
(owner t1)
(ask p1 'lose t1)
(ask soda 'appear t1)
(owner t1)

(ask brian 'description)

;;A3
(define A-119 (instantiate place 'A-119))
(define waitin2010 (instantiate person 'waitin2010 A-119))
(ask a-119 'set-desc "The Netword lab in PKU")
(ask waitin2010 'description)


;;A4a
(define A-119 (instantiate place 'A-119))
(define waitin2010 (instantiate person 'waitin2010 A-119))
(ask waitin2010 'set-talk "my name is waitin2010")
(define xiarui (instantiate person 'xiarui A-119))
(ask xiarui 'set-talk "oh, my god!")
(define qiuchanghe (instantiate person 'qiuchanghe A-119))

;;A4b
(define A-119 (instantiate place 'A-119))
(define waitin2010 (instantiate person 'waitin2010 A-119))
(ask waitin2010 'set-talk "my name is waitin2010")
(ask waitin2010 'add-notice-proc (lambda (person) (print "Please take a flyer!")))
(ask waitin2010 'add-notice-proc (lambda (person) 
				   (print (se 'Place: (name (ask person 'place))))
				   (define flyer (instantiate thing 'flyer))

				   (ask person 'take flyer)))
(ask waitin2010 'add-notice-proc (lambda (person)
				   (ask (ask person 'place) 'appear flyer)
				   (print (se 'appear (ask flyer 'name)))))
(define xiarui (instantiate person 'xiarui A-119))
(ask xiarui 'set-talk "oh, my god!")
(define qiuchanghe (instantiate person 'qiuchanghe A-119))


;;B1
(define coffee1 (instantiate thing 'coffee))
(define coffee2 (instantiate thing 'coffee))
(define coffee3 (instantiate thing 'coffee))
(ask bh-office 'appear coffee1)
(ask bh-office 'appear coffee2)
(ask bh-office 'appear coffee3)
(inventory bh-office)
(define waitin2010 (instantiate person 'waitin2010 bh-office))

(whereis waitin2010)
(ask waitin2010 'take coffee1)
(inventory waitin2010)
