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

(define-tests macro-shadowing-tests
  ((begin
     (define foo (lambda () 'procedure))
     (define-syntax foo
       (syntax-rules ()
	 ((foo) 'macro)))
     (foo)) => macro)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo) 'macro)))
     (define foo (lambda () 'procedure))
     (foo)) => procedure)
  ((begin
     (define (foo) 'procedure)
     (define-syntax foo
       (syntax-rules ()
	 ((foo) 'macro)))
     (foo)) => macro)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo) 'macro)))
     (define (foo) 'procedure)
     (foo)) => procedure)
)

(define-tests macro-nonliteral-matching-tests
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x) 'nonliteral-id))))
     (foo foo)) => nonliteral-id)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo y) (+ y y)))))
     (foo 100)) => 200)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x) "hi"))))
     (foo (1 2))) => "hi")
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x) x))))
     (foo "hi")) => "hi")
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x) x))))
     (foo '(1 2))) => (1 2))
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x y) (+ x y)))))
     (foo 3 4)) => 7)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo x y) '(x x x y))))
     (foo 3 4)) => (3 3 3 4))
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x y) (quote (x x x y))))))
     (foo 3 4)) => (3 3 3 4))
)

(define-tests macro-literal-matching-tests
  ((let-syntax
       ((foo (syntax-rules (x)
	       ((foo x) 'literal-id))))
     (foo x)) => literal-id)
  ((begin
     (define x 1)
     (define-syntax foo
       (syntax-rules (x)
	 ((foo x) 'literal-id)))
     (foo x)) => literal-id)
)

(define-tests macro-list-matching-tests
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (a b c)) c))))
     (foo (1 2 3))) => 3)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (((((x)))))) x))))
     (foo ((((('five))))))) => five)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo ((((x))))) x))))
     (foo (((('four)))))) => four)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (((x)))) x))))
     (foo ((('three))))) => three)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo ((x))) x))))
     (foo (('two)))) => two)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (x)) x))))
     (foo ('one))) => one)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (a (b (c (d)))))
	    (+ a b c d)))))
     (foo (1 (2 (3 (4)))))) => 10)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo ((((a) b) c)))
	  (/ a b c))))
     (foo ((((12) 2) 3)))) => 2)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x y)
	    (+ x (* 2 y))))))
     (foo 3 4)) => 11)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo (x) (y))
	  (+ x (* 2 y)))))
     (foo (3) (4))) => 11)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (a b) (c d))
	    (+ a c)))))
     (foo (1 2) (3 4))) => 4)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo (x y z))
	  (quote (x y . z)))))
     (foo (a b c))) => (a b . c))
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (x y z))
	    (quote #(x y z))))))
     (foo (a b c))) => #(a b c))
)

(define-tests macro-improper-list-matching-tests
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo (x . y))
	  (/ y x))))
     (foo (2 . 1024))) => 512)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (x y . z))
	    (+ x y z)))))
     (foo (10 11 . 12))) => 33)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo (a . b) (c . d))
	  (/ a b c d))))
     (foo (1024 . 2) (4 . 8))) => 16)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (a . (b . (c . d))))
	    (/ a b c d)))))
     (+ (foo (100 . (2 . (5 . 2)))) 100)) => 105)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo (((a . b) . c) . d))
	  (/ d c b a))))
     (foo (((2 . 3) . 5) . 60))) => 2)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (x . y))
	    'ok))))
     (foo (1 2))) => ok)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo (x . y))
	  y)))
     (foo (1 . 2))) => 2)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (x . y))
	    (quote y)))))
     (foo (1 2))) => (2))
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo (x . y))
	  (quote y))))
     (foo (1 2 3 (4 5)))) => (2 3 (4 5)))
)

(define-tests macro-ellipsis-matching-tests
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x ...)
	    (quote (x ...))))))
     (foo 1 2)) => (1 2))
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo x ...)
	  (x ...))))
     (foo + 1 2 3)) => 6)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo x ...)
	    (+ x ...)))))
     (foo)) => 0)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo x ...)
	  (+ x ...))))
     (foo)) => 0)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (x ...) (y ...))
	    (+ x ... y ...)))))
     (foo (1 2 3) (4 5 6))) => 21)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo (x ...) (y ...))
	  (+ x ... y ...))))
     (foo () ())) => 0)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (x y ...) (z w ...))
	    (+ y ... w ...)))))
     (foo ('not-a-number) ('not-a-number-either))) => 0)
)

(define-tests macro-vector-matching-tests
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo #(a b c))
	  c)))
     (foo #(1 2 3))) => 3)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo #(#(#(#(#(x)))))) x))))
     (foo #(#(#(#(#('five))))))) => five)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo #(#(#(#(x))))) x)))
     (foo #(#(#(#('four)))))) => four)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo #(#(#(x)))) x))))
     (foo #(#(#('three))))) => three)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo #(#(x))) x)))
     (foo #(#('two)))) => two)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo #(x)) x))))
     (foo #('one))) => one)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo #(a (b #(c (d)))))
	  (+ a b c d))))
     (foo #(1 (2 #(3 (4)))))) => 10)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo #((((a) b) c)))
	    (/ a b c)))))
     (foo #((((12) 2) 3)))) => 2)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo #(x) #(y)) (+ x (* 2 y)))))
     (foo #(3) #(4))) => 11)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo #(a b) #(c d)) (+ a c)))))
     (foo #(1 2) #(3 4))) => 4)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo #(x y z))
	  (quote (x y . z)))))
     (foo #(a b c))) => (a b . c))
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo #(x y z))
	    (quote #(y z x))))))
     (foo #(a b c))) => #(b c a))
)

(define-tests macro-nested-ellipses-tests
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo #(x ...) ...)
	  (+ (* x ...) ...))))
     (foo #(1 2 3) #(4 5) #())) => 27)
  ((let-syntax
       ((foo
	 (syntax-rules ()
	   ((foo (x ...) ...)
	    (+ (* x ...) ...)))))
     (foo (1 2 3) (4 5) ())) => 27)
)

(define-tests macro-definition-environment-tests
  ((begin
     (define x 1)
     (define-syntax foo
       (syntax-rules ()
	 ((foo) x)))
     ((lambda (x) (foo)) 2)) => 1)
  ((let ((x 1))
     (let-syntax ((foo (syntax-rules () ((foo) x))))
       (define (bar x)
	 (+ x (foo)))
       (bar 2))) => 3)
  ((begin
     (define x 1)
     (define-syntax foo
       (syntax-rules ()
	 ((foo) x)))
     (define (bar x)
       (+ x (foo) x))
     (bar 2)) => 5)
  ((let ((x 1))
     (let-syntax ((foo (syntax-rules () ((foo) x))))
       (let ((bar (lambda (x) (+ (foo) x (foo)))))
	 (bar 2)))) => 4)
  ((begin
     (define-syntax foo
       (syntax-rules ()
	 ((foo) x)))
     (define x 'whew)
     (foo)) => whew)
)

(define-tests macro-id-collision-tests
  ((let-syntax
       ((foo ; this is basically the definition of or
	 (syntax-rules ()
	   ((foo) #f)
	   ((foo x) x)
	   ((foo x y ...)
	    (let ((z x))
	      (if z z (foo y ...)))))))
     (let ((z 32))
       (foo z #f))) => 32)
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