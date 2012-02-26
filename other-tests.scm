(define-tests sanity-checks
  (1 => 1)
  ((+ 1 1) => 2)
  ('(1 1) => (1 1))
  ('(1 . 1) => (1 . 1))
  ((let ((x 1)) (+ x x)) => 2)
  ((let ((x 1) (y 2)) (+ y x)) => 3)
  ((begin
     (define (foo x y) (+ x (* 2 y)))
     (foo 3 4)) => 11)
  ((let ((foo (lambda (x y) (+ x (* 2 y)))))
     (foo 3 4)) => 11)
  ((begin
     (define (foo) "hi")
     (define bar (foo))
     bar) => "hi")
  ((let* ((foo (lambda () "hi"))
	  (bar (foo)))
     bar) => "hi")
  ((begin
     (define (foo x . y) y)
     (foo 3 4 5)) => (4 5))
  ((begin
     (define (foo x) (* x x))
     (+ (foo 3) (foo 4))) => 25)
  ((let ((foo (lambda (x) (* x x))))
     (+ (foo 3) (foo 4))) => 25)
  ((begin
     (define (fac n)
       (if (= n 0)
	   1
	   (* n (fac (- n 1)))))
     (fac 10)) => 3628800)
  ((begin
     (define (tail-fac n buf)
       (if (= n 0)
	   buf
	   (tail-fac (- n 1) (* buf n))))
     (define (fac n) (tail-fac n 1))
     (fac 10)) => 3628800)
  ((begin
     (define (tail xs)
       (if (null? (cdr xs))
	   (car xs)
	   (tail (cdr xs))))
     (tail '(1 2 (3 4 5)))) => (3 4 5))
  ((((lambda (x) x) (lambda (y) y)) "hello!") => "hello!")
  ((begin
     (define x 1)
     (define y 2)
     (+ x y)) => 3)
  ((begin
     (define x 1)
     (define y 1)
     (set! x (+ x 100))
     (set! y (+ x 100))
     (+ x y)) => 302)
  (((lambda x x) 32) => (32))
  ((((lambda (x) +) 3) 100 1) => 101)
  ((((lambda (x) (lambda (y) (/ x y))) 10) 4) => 2.5)
  ((begin
     (define (div-me x)
       (lambda (y) (/ x y)))
     ((div-me 10) 4)) => 2.5)
  (((lambda (x)
      ((lambda (y)
	 ((lambda (z) (+ x y z z z))
	  3))
       2))
    1) => 12)
  ((string? (make-string 0)) => #t)
  ((string-length (make-string 4)) => 4)
  ((string-ref "hello!" 4) => #\o)
  ((car '(x y)) => x)
  ((car '(x . y)) => x)
  ((cdr '(x y)) => (y))
  ((cdr '(x . y)) => y)
  ((car '(x)) => x)
  ((cdr '(x)) => ())
  ((caar '((1 2) . 3)) => 1)
  ((cadr '((1 2) 3)) => 3)
  ((begin
     (define (foo)
       (define x 'dynamic-scoping)
       x)
     (define x 'lexical-scoping)
     (foo)
     x) => lexical-scoping)
  (((lambda (x)
      ((lambda (y)
	 (+ x (* 2 y)))
       100))
    2) => 202)
  ((equal? '(1 2) '(1 . 2)) => #f)
  ((equal? '(1 2) '(1 2)) => #t)
  ((if (even? 4201)
       'even
       'odd) => odd)
  ((if (even? 4200)
       'even
       'odd) => even)
  ((begin
     (define (foo x)
       (begin
	 (define x 1)
	 (define y 2))
       (+ x y))
     (foo 32)) => 3)
  ((begin
     (define x 1)
     x) => 1)
  ((begin
     (begin
       (define x 1))
     x) => 1)
  ((begin
     (begin
       (begin
	 (define x 1))
       x)) => 1)
  ((+ '1 2) => 3)
  ((+ `1 2) => 3)
  ((number? 10) => #t)
  ((number? '10) => #t)
  ((number? `10) => #t)
  ((number? ''10) => #f)
  ((number? ``10) => #f)
  ((number? '`10) => #f)
  ((number? `'10) => #f)
  ((car `(,(+ 1 2) ,(+ 3 4))) => 3)
  ((cdr `(,(+ 1 2) ,(+ 3 4))) => (7))
  (`,(+ 1 100) => 101)
  ((+ `,(+ 1 100) 10) => 111)
  (`(1 2 ,@(list 3 4)) => (1 2 3 4))
  (`(1 2 ,@(list)) => (1 2))
  ((begin
     (define (foo x)
       (define (bar) x)
       (bar))
     (foo 'x)
     (foo 'y)) => y)
  ((begin
     (define (foo x)
       (let ((bar (lambda () x)))
	 (bar)))
     (foo 'x)
     (foo 'y)) => y)
  ((let () 1 2 3) => 3)
  ((let () (define x 2) (+ x x)) => 4)
  ((letrec
       ((my-even?
	 (lambda (n)
	   (if (= n 0)
	       #t
	       (my-odd? (- n 1)))))
	(my-odd?
	 (lambda (n)
	   (if (= n 0)
	       #f
	       (my-even? (- n 1))))))
     (my-even? 15)) => #f)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo x ...)
	  (cons (list x ...) (list x ...)))))
     (foo 1 2 3)) => ((1 2 3) 1 2 3))
  ((let-syntax
       ((foo (syntax-rules ()
	       ((foo x ...)
		(cons (list x ...) (list x ...))))))
     (foo 1 2 3)) => ((1 2 3) 1 2 3))
  ((begin
     (define x 'wrong)
     ((lambda ()
	(define x 'right)
	x))) => right)
  ((begin
     (letrec ((x 1)) x)
     (letrec () 1)
     (letrec ((x 2)) x)) => 2)
  ((let-syntax () 1) => 1)
  ((letrec-syntax () 1) => 1)
  ((let-syntax
       ((foo (syntax-rules ()
	       ((foo x) 'hi))))
     (foo ())) => hi)
  ((letrec-syntax
       ((foo (syntax-rules ()
	       ((foo x) 'hi))))
     (foo ())) => hi)
  ((begin
     (define (run f x) (f x))
     (define (autorun) (run even? 32))
     (autorun)
     (autorun)) => #t)
  ((let ((foo (display "")))
     foo
     32) => 32)
  ((dynamic-wind
       (lambda () 1)
       (lambda () 2)
       (lambda () 3)) => 2)
  ((begin
     (define (compose g f)
       (lambda (x) (g (f x))))
     (define double (lambda (x) (* x 2)))
     ((compose double double) 3)) => 12)
  ((begin
     (define counter 0)
     (define (foo x)
       (set! counter (+ counter 1))
       (if (= x 0)
	   'done
	   (foo (- x 1))))
     (foo 3)
     (set! counter 0)
     (foo 3)
     counter) => 4)
  ((string-append
    "when "
    "in "
    "the "
    "course "
    "of "
    "human "
    "events")
   => "when in the course of human events")
  ((begin
     (define classify
       (lambda (l r)
	 (let ((evens (car l))
	       (odds  (cdr l)))
	   (if (even? r)
	       (cons (+ evens 1) odds)
	       (cons evens (+ odds 1))))))
     (foldl classify (cons 0 0) '(10 12 20 11 17 13 4)))
   => (4 . 3))
  ((force (delay (+ 100 100))) => 200)
  ((begin
     (define x (delay (+ 100 100)))
     (list (force x) (force x))) => (200 200))
  ((force (force (delay (delay (+ 1 2))))) => 3)
  ((map (lambda (x) (+ x x)) '(2 3 4 5)) => (4 6 8 10))
  ((map +
	'(1 2 3)
	'(4 5 6)
	'(7 8 9)
	'(10 11 12)) => (22 26 30))
  ((map +
	(list 1 2 3)
	(list 4 5 6)
	(list 7 8 9)
	'(10 11 12)) => (22 26 30))
  ((begin
     (define (foo x)
       (define (bar y) (+ x y))
       bar)
     ((foo 10) 100)) => 110)
  ((begin
     (define (foo x)
       (lambda (y) (+ x y)))
     ((foo 10) 100)) => 110)
  ((let
       ((v (make-vector 5)))
     (for-each
      (lambda (i)
	(vector-set! v i (* i i)))
      '(0 1 2 3 4))
     v) => #(0 1 4 9 16))
  ((begin
     (define x 1)
     (define y 2)
     (or x y)) => 1)
  ((let* ((x '(1 2 3 4))
	  (y (cdr x))
	  (z (cdr y)))
     (set-car! z 100)
     (and
      (equal? x '(1 2 100 4))
      (equal? y '(2 100 4))
      (equal? z '(100 4)))) => #t)
  ((let* ((x '(1 2 3 4))
	  (y (cdr x))
	  (z (cdr y)))
     (set-cdr! z 100)
     (and
      (equal? x '(1 2 3 . 100))
      (equal? y '(2 3 . 100))
      (equal? z '(3 . 100)))) => #t)
)
)

(define-tests cyclicity-tests
  ((begin
     (define x (list 1 2))
     (set-car! x x)
     (list? x)) => #t)
  ((begin
     (define x (list 1 2))
     (set-cdr! x x)
     (list? x)) => #f)
  ((begin
     (define x (list 1 2 3 4))
     (define y (cdr x))
     (set-cdr! y y)
     (or (list? x) (list? y))) => #f)
  ((let* ((x (list 1 2 3 4))
	  (y (cdr x))
	  (z (cdr y)))
     (set-cdr! z z)
     (or (list? x) (list? y) (list? z))) => #f)
)