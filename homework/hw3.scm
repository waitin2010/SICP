(define pi 3.141592654)

(define (make-square side)
  (attach-tag 'square side))

(define (make-circle radius)
  (attach-tag 'circle radius))

(define (area shape)
  (cond ((equal? (type-tag shape) 'square)
		 (* (contents shape) (contents shape)))
		((equal? (type-tag shape) 'circle)
		 (* pi (contents shape) (contents shape)))
		(else (error "Unknown shape -- AREA"))))

(define (perimeter shape)
  (cond ((equal? (type-tag shape) 'square)
		 (* 4 (contents shape)))
		((equal? (type-tag shape) 'circle)
		 (* 2 pi (contents shape)))
		(else (error "Unknown shape -- PERIMETER"))))

#|
	The above procedures implement a dispatch on type system for
	 calculating the area and perimeter of shapes.
	This is a reasonable solution for a system that is likely to encounter
	 the addition of more methods/operators (eg. a number-of-sides
	 procedure) than types (eg. triangles, rectangles).
	But what if we wanted a system that is more conducive to adding types?

	Specifically, I want to define the area and perimeter procedures as follows:
|#

(define (area shape)
  (shape 'area))
(define (perimeter shape)
  (shape 'perimeter))

#|
a)	Re-implement the above procedures in message passing style to work with the
	given, generalized area and perimeter procedures.
|#


#|
b)	Now, add a triangle to the message passing system you've created.
	You should NOT need to modify any of the code you've written for part (a)!
|#

#|
	A 2D rectangular cartesian coordinate system allows points to be represented
	as pairs of x and y values.

a)	Write a make-rectangular-point procedure to serve as a point representation in
	message	passing style. A point should accept the messages 'x, 'y and throw an
	error if the message is anything else.

b)	Add support for the distance message for your points.z
	(my-point 'distance) should return a procedure that takes another point
	as an argument and returns the distance between the two points.

c)	Points in a plane may also be represented in polar form, using a magnitude (r)
	and an angle (theta). Conversions from polar to rectangular coordinates use
	the equations: x = r * cos(theta); y = r * sin(theta).
	Write an analogous make-polar-point procedure that takes as input a magnitude
	and an angle. A polar point should accept the 'x and 'y messages and return
	corresponding rectangular coordinates.

d)	Make a rectangular point and a polar point. Show how you would find the
	distance between the two points using what you have from the previous parts
	of the question.
|#

;; Sample Library Data
(define moffitt-collection
  (list (attach-tag 'moffitt '((anna karenina) (leo tolstoy) 0143035002))
		(attach-tag 'moffitt '((computer science logo style) (brian harvey) 0262580721))
		(attach-tag 'moffitt '((to the lighthouse) (virginia woolf) 0679405372))
		(attach-tag 'moffitt '((notes from the underground) (feodor dostoevsky) 1453600620))))

(define doe-collection
  (list (attach-tag 'doe (list (cons 'title '1984)
							   (cons 'author '(george orwell))
							   (cons 'isbn '0143035002)))
		(attach-tag 'doe (list (cons 'title '(animal farm))
							   (cons 'author '(george orwell))
							   (cons 'isbn '1412811902)))
		(attach-tag 'doe (list (cons 'title '(to kill a mockingbird))
							   (cons 'author '(harper lee))
							   (cons 'isbn '1412811902))) ))

(put 'moffitt 'title (lambda (book-record) (car book-record)))
(put 'moffitt 'author (lambda (book-record) (cadr book-record)))
(put 'moffitt 'isbn (lambda (book-record) (caddr book-record)))

(put 'doe 'title (lambda (book-record) (cdr (assoc 'title book-record))))
(put 'doe 'author (lambda (book-record) (cdr (assoc 'author book-record))))
(put 'doe 'isbn (lambda (book-record) (cdr (assoc 'isbn book-record))))

(define all-library-collections (list moffitt-collection doe-collection))

#|
The University of California, Berkeley Library is made up of a group of
smaller libraries, each of which uses Scheme data structures to represent
its collection. Unfortunately, each library has its own method of
cataloguing books (using pairs, lists, nested association lists, etc.).
Each library is required to store a book's title, author and ISBN
number. As the new head of IT for the centralized library system, you need
to provide the librarians with a common method of accessing this data, and
you decide to use Data-Directed Programming to accomplish this.

You will be writing generic operators so that your program can work with any type of information.
A generic operator is a procedure that can take in data of different types and is able to perform its operation on any of these types.
|#

#|
For example, Moffitt Library implements its books as a list, like:
((anna karenina) (leo tolstoy) 0143035002)

On the other hand, the Doe Library implements them as an association list. This means that they do them like this:
((title . 1984) (author . Orwell) (isbn . 0151010269))

We call each of these a book record. Note that we need different accessors to get the each element. For example, for Moffitt, they say:
(define (author book)
  (cadr book))

Doe, on the other hand, says:
(define (author book)
  (cdr (assoc author book)))

Implement a GET-BOOK-RECORD procedure that, given a library's data file and
book title, retrieves the corresponding book record. This procedure must work for any
library's data files. Each data file is a list of book records.
|#


#|
Assume that each library has called attach-tag for each piece of data belonging to it, as in the sample data sets. For example, Moffitt has called (attach-tag 'Moffitt book-record). Furthermore, all the libraries have added all the information to a table.

Implement a GET-ISBN-NUMBER procedure that returns the ISBN number from a
given book record from any library's data file.
|#


#|
Write a FIND-BOOK procedure that, given a book title and a list of the book records of all of the libraries, retrieves the corresponding book record.
|#


#|
The Berkeley Public Library (on Shattuck Ave) decides to join the UC Berkeley
Library system. What do they need to do to put their database on the central system?
|#


#|
SICP Exercise 2.76, 2.77, 2.79, 2.80.
|#

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
            (sqrt (+ (square (real-part z))
                     (square (imag-part z)))))
    (define (angle z)
            (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a) 
            (cons (* r (cos a)) (* r (sin a))))
    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular 
      (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular 
      (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-everything)
  (install-complex-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-rectangular-package)
  (install-polar-package))
#|
SICP Exercises 2.81, 2.83
|#
