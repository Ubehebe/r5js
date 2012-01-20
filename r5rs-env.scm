; This file contains definitions for all non-primitive procedures in R5RS
; listed as "procedure" or "library procedure".

(define (not p) (if p #f #t))

(define (equal? x y)
  (if (and (pair? x) (pair? y))
      (and (equal? (car x) (car y))
	   (equal? (cdr x) (cdr y)))
      (eqv? x y)))

(define (member-abstract are-equal?)
  (lambda (x ys)
    (if (null? ys) 
	#f
	(if (are-equal? x (car ys))
	    ys
	    ((member-abstract are-equal?) x (cdr ys))))))

(define memq (member-abstract eq?))
(define memv (member-abstract eqv?))
(define member (member-abstract equal?))

(define (zero? z) (= z 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (odd? n) (= 1 (remainder n 2)))
(define (even? n) (= 0 (remainder n 2)))

(define (reduce f xs init)
  (if (null? xs)
      init
      (reduce f (cdr xs) (f init (car xs)))))

(define (max x . ys)
  (if (null? ys)
      x
      (reduce
       (lambda (y z) (if (< y z) z y))
       ys
       x)))

(define (min x . ys)
  (if (null? ys)
      x
      (reduce
       (lambda (y z) (if (< y z) y z))
       ys
       x)))

(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (compose f g) (lambda (x) (f (g x))))

(define caar (compose car car))
(define cadr (compose car cdr))

(define (list . xs) xs) ; haha
