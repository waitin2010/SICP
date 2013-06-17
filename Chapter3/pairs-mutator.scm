;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Project:    pairs mutators
;;;;;;;;Who:        waitin2010
;;;;;;;;Date:       2013-05-03
;;;;;;;;Description: mutators can modify the pairs

(define x '((a b) c d))
(define y '(e f))
(set-car! x y)
x
;((e f) c d)
(define z (cons y (cdr x)))
z
;((e f) c d)
(define x '((a b) c d))
(set-cdr! x y)
x
;((a b) e f)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z
;(a b c d)
(cdr x)
;(b)
(define w (append! x y))
w
;(a b c d)
(cdr x)
;(b c d)
