;add repeat method, which repeats the last thng said.

;Answer: use the local state technology, that is a instance variable in the oop
(define-class (person name)
  (instance-vars (sent '()))
  (method (say stuff) (set! sent stuff) sent)
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name))) 
  (method (repeat) sent))

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat)))) 



(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se stuff stuff)))


(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (usual 'say (se stuff stuff))))



;test for the double talker
(define mike (instantiate double-talker 'mike))
(ask mike 'say '(hello))
;(hello hello)
(ask mike 'say '(the sky is falling))
;(the sky is falling the sky is falling)
