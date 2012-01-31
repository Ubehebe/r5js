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

(define (foldl f start xs)
  (if (null? xs)
      start
      (foldl f (f start (car xs)) (cdr xs))))

(define (length xs)
  (foldl
   (lambda (x y) (+ x 1))
   0
   xs))

(define (max x . ys)
  (if (null? ys)
      x
      (foldl
       (lambda (y z) (if (< y z) z y))
       x
       ys)))

(define (min x . ys)
  (if (null? ys)
      x
      (foldl
       (lambda (y z) (if (< y z) y z))
       x
       ys)))

(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (compose . fs)
  (define (compose-list fs)
    (if (null? fs)
	(lambda (x) x)
	(let ((f (car fs))
	      (g (compose-list (cdr fs))))
	  (lambda (x) (f (g x))))))
  (compose-list fs))

(define caar (compose car car))
(define cadr (compose car cdr))
(define cdar (compose cdr car))
(define cddr (compose cdr cdr))

(define caaar (compose car car car))
(define caadr (compose car car cdr))
(define cadar (compose car cdr car))
(define caddr (compose car cdr cdr))
(define cdaar (compose cdr car car))
(define cdadr (compose cdr car cdr))
(define cddar (compose cdr cdr car))
(define cdddr (compose cdr cdr cdr))

(define caaaar (compose car car car car))
(define caaadr (compose car car car cdr))
(define caadar (compose car car cdr car))
(define caaddr (compose car car cdr cdr))
(define cadaar (compose car cdr car car))
(define cadadr (compose car cdr car cdr))
(define caddar (compose car cdr cdr car))
(define cadddr (compose car cdr cdr cdr))
(define cdaaar (compose cdr car car car))
(define cdaadr (compose cdr car car cdr))
(define cdadar (compose cdr car cdr car))
(define cdaddr (compose cdr car cdr cdr))
(define cddaar (compose cdr cdr car car))
(define cddadr (compose cdr cdr car cdr))
(define cdddar (compose cdr cdr cdr car))
(define cddddr (compose cdr cdr cdr cdr))

(define (list . xs) xs)

(define (append xs ys)
  (if (null? (cdr xs))
      (cons (car xs) ys)
      (cons (car xs) (append (cdr xs) ys))))

(define (flip f) (lambda (x y) (f y x)))

(define (reverse xs) (foldl (flip cons) '() xs))

