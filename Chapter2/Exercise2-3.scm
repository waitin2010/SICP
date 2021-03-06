;Function: represent line segments in a plane
;Date: 2013-03-11 16:39
;Who: waitin2010

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define start (make-point 10 10))
(define end (make-point 20 20))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
		    (x-point (end-segment segment)))
		 2)
	      (/ (+ (y-point (start-segment segment))
		    (y-point (end-segment segment)))
		 2)))
(define (midpoint-segment-answer s)
  (define (average a b) (/ (+ a b) 2))
  (let ((s (start-segment s)) (e (end-segment s)))
    (make-point (average (x-point s) (x-point e))
		(average (y-point s) (y-point e)))))

(define (square x) (* x x))

(define (length-segment segment)
  (let ((s (start-segment segment))
	(e (end-segment segment)))
    (sqrt (+ (square (- (x-point s)
			(x-point e)))
	     (square (- (y-point s)
			(y-point e)))))))
(define (print-segment s)
  (newline)
  (display "start-point:")
  (print-point (start-segment s))
  (newline)
  (display "end-point:")
  (print-point (end-segment s))
  (newline))
(define segment (make-segment start end))

(define (make-rectangle segment width) (cons segment width))
(define (make-rectangle2 segment alpha)
  (let ((width (* (tan alpha) (length-segment segment))))
    (make-rectangle segment width)))
(define (segment-rectangle rect) (car rect))
(define (width-rectangle rect) (cdr rect))

(define (area rectangle)
  (* (length-segment (segment-rectangle rectangle))
     (abs (width-rectangle rectangle))))

(define (perimeter rectangle)
  (* (+ (length-segment (segment-rectangle rectangle))
	(abs (width-rectangle rectangle)))
     2))

(define width 3)
(define rectangle (make-rectangle segment width))
