;;;;Date: 2013-03-21
;;;;Who: waitin2010
(list 'a 'b 'c)
;Answer: (a b c)
(list (list 'george))
;Answer: ((george))
(cdr '((x1 x2) (y1 y2)))
;Answer: ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;Answer: (y1 y2)
(pair? (car '(a short list)))
;Answer: #f
(memq 'red '((red shose) (blue socks)))
;Answer: #f
(memq 'red '(red shoes blue scoks))
;Answer: (red shoes blue socks)