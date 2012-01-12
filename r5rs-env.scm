; This file contains definitions for all procedures in R5RS listed as
; "procedure" or "library procedure".

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