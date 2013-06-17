;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.
(define-class (basic-object)
  (instance-vars (properties (make-table)))
  (method (put key value)
	  (insert! key value properties))
  (method (get key)
	  (lookup key properties)))
  
(define-class (place name)
  (parent (basic-object))
  (instance-vars
   (directions-and-neighbors '())
   (exit-list '())
   (things '())
   (people '())
   (descrip '()) ;; added by waitin2010 in 20130606 for A3
   (entry-procs '())
   (exit-procs '()))
  
  (initialize
   (ask self 'put 'place? #t)
   )

  (method (set-desc! descrp)
	  (set! descrip descrp))
  (method (description) descrip)
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction exit-list)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cddr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (set! people (cons new-person people))
    (if (not (null? people))
	(for-each (lambda (who) 
			(ask who 'notice new-person)) (cdr people)))

    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit? direction person)
	  (let ((lock (cadr (assoc direction exit-list))))
	    (if (eq? lock not-locked)
		#t
		(ask lock 'unlock person))))
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)

  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)
  (method (new-neighbor2 direction neighbor itemlock)
	  (if (assoc direction exit-list)
	      (error "Direction already assigned a neighor" (list name direction)))
	  (set! exit-list
		(cons (cons direction (cons itemlock neighbor)) exit-list))
	  'connected)

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) )

(define-class (person name place)
  (parent (basic-object))
  (instance-vars
   (on-notice-procs '()) ;;added by waitin2010 in 20130607
   (possessions '())
   (saying ""))
  (initialize
   (ask place 'enter self)
   (ask self 'put 'strength 2)
   (ask self 'put 'health 2)
   (ask self 'put 'max-health 10)
   (ask self 'put 'steps 0)
   (ask self 'put 'person? #t)
   )
  (method (eat)
	  (let ((foods (filter (lambda (thing) (ask thing 'get 'ediable?)) possessions)))
	    (map (lambda (food) (ask self 'increase-health (ask food 'calories))) foods)
	    (set! possessions (filter (lambda (thing) (not (ask thing 'get 'ediable?))) possessions))))


  (method (update-steps)
	  (let ((steps (+ (ask self 'get 'steps) 1)))
	    (cond ((= (remainder steps 7) 0)
		   (begin (ask self 'put 'strength (+ 1 (ask self 'get 'strength))) (print "You feel stronger")))
		  ((= (remainder steps 11) 0)
		   (ask self 'put 'max-health (+ 5 (ask self 'get 'max-health)))
		   (ask self 'put 'health (ask self 'get 'max-health))
		   (print "your max health increase by 5"))
		  (else (ask self 'put 'steps steps)
			(print '(Today you walk))))))
  (method (increase-health num)
	  (let ((max-health (ask self 'get 'max-health))
		(health (ask self 'get 'health)))
	    (if (> (+ health num) max-health)
		(begin (ask self 'put 'health max-health) (- max-health health))
		(begin (ask self 'put 'health (+ health num)) num))))
  (method (decrease-health num)
	  (let ((health (ask self 'get 'health)))
	    (if (< (- health num) 0)
		(ask self 'faint)
		(ask self 'put 'health (- health num)))))
  (method (faint)
	 (ask place 'exit self)
	 (map (lambda (possessor) (ask self 'lose possessor)) possessions)
	 (set! place 'somewhere))

  (method (take-all)
	  (let ((things (ask place 'things)))
	    (let ((things-no-one (filter (lambda (thing) (equal? 'no-one (ask thing 'possessor))) things)))
	      (print things)
	      (print things-no-one)
	      (map (lambda (thing) (ask self 'take thing)) things-no-one)))) 
  (method (add-notice-proc proc)
	  (set! on-notice-procs (append on-notice-procs (list proc)))) ;;added by waitin2010 in 20130607
  (method (clear-notice-proc)
	  (set! on-notice-procs '())
	  'cleared) ;;added by waitin2010 in 20130607
  (method (description)
	  (se name 'is 'in (ask (ask self 'place) 'description)))
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (Method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
	  (else
	   (announce-take name thing)
	   (set! possessions (cons thing possessions))
	       
	   ;; If somebody already has this object...
	   (for-each
	    (lambda (pers)
	      (if (and (not (eq? pers self)) ; ignore myself
		       (memq thing (ask pers 'possessions)))
		  (begin
		   (ask pers 'lose thing)
		   (have-fit pers))))
	    (ask place 'people))
	       
	   (ask thing 'change-possessor self)
	   'taken)))

  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) 
	  (ask self 'talk)
	  (for-each (lambda (proc) (proc person)) on-notice-procs)

	  )
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
	     (error "Can't go" direction))
	    ((not (ask place 'exit? direction self))
	     "you cannot exit from this direction")
	    (else
	     (ask place 'exit self)
	     (ask self 'update-steps)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self))
	    ))))

#|(define thing
  (let ()
    (lambda (class-message)
      (cond
       ((eq? class-message 'instantiate)
	(lambda (name)
	  (let ((self '()) (possessor 'no-one))
	    (define (dispatch message)
	      (cond
	       ((eq? message 'initialize)
		(lambda (value-for-self)
		  (set! self value-for-self)))
	       ((eq? message 'send-usual-to-parent)
		(error "Can't use USUAL without a parent." 'thing))
	       ((eq? message 'name) (lambda () name))
	       ((eq? message 'possessor) (lambda () possessor))
	       ((eq? message 'type) (lambda () 'thing))
	       ((eq? message 'change-possessor)
		(lambda (new-possessor)
		  (set! possessor new-possessor)))
	       (else (no-method 'thing))))
	    dispatch)))
       (else (error "Bad message to class" class-message))))))
|#
(define-class (thing name)
  (parent (basic-object))
  (instance-vars 
    (possessor 'no-one))
  (initialize
   (ask self 'put 'thing? #t)
   )
  (method (type) 'thing)
  (method (thing?) #t)
  (method (send-usual-to-parent)
	  (error "Can't use USUAL without a parent." 'thing))
  (method (change-possessor new-possessor)
	  (set! possessor new-possessor)))
(define cup (instantiate thing 'cup))
(ask cup 'name)
(ask cup 'possessor)
(ask cup 'change-possessor 'hello)
(ask cup 'possessor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  (member? (ask thing 'name) *foods*))
(define-class (food name calories)
  (parent (thing name))
  (initialize
   (ask self 'put 'ediable? #t)
   )
  )

(define-class (bagel name calories)
  (parent (food name calories))
  (class-vars (names 'bagel))
  )
(define-class (lock)
  (instance-vars (state #t))
  (method (unlock)
	  (set! state #f)
	  'unlock))
(define-class (item-lock  key)
  (parent (lock))
  (instance-vars (msg "you must have a key"))
  (method (set-correct-msg message)
	  (set! msg message))
  (method (change-key new-key)
	  (set! key new-key))
  (method (unlock person)
	  (if (null? (filter (lambda (thing) (eq? thing key)) (ask person 'possessions)))
	      (begin (print msg) #f)
	      (begin (usual 'unlock) (ask person 'lose key) #t))))

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)

  (method (notice person)
    (if (eq? behavior 'run)
	(ask self 'go (pick-random (ask (usual 'place) 'exits)))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) )))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))

(define (can-go from direction to itemlock)
  (ask from 'new-neighbor2 direction to itemlock))

(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (ask obj 'get 'person?)))

(define (thing? obj)
  (and (procedure? obj)
       (ask obj 'get 'thing?)))
