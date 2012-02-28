; This file lists every example in the R5RS standard that should evaluate
; successfully to a specified value.

(define-tests evaluation-examples-1.3.4
  ((* 5 8) => 40)
  )

(define-tests variable-references-4.1.1
  ((begin (define x 28) x) => 28)
  )

(define-tests literal-expressions-4.1.2
  ((quote a) => a)
  ((quote #(a b c)) => #(a b c))
  ((quote (+ 1 2)) => (+ 1 2))
  ('a => a)
  ('#(a b c) => #(a b c))
  ('() => ())
  ('(+ 1 2) => (+ 1 2))
  ('(quote a) => (quote a))
  (''a => (quote a))
  ('"abc" => "abc")
  ("abc" => "abc")
  ('145932 => 145932)
  (145932 => 145932)
  ('#t => #t)
  (#t => #t)
  )

 (define-tests procedure-calls-4.1.3
   ((+ 3 4) => 7)
   (((if #f + *) 3 4) => 12)
   )

(define-tests procedures-4.1.4
  (((lambda (x) (+ x x)) 4) => 8)
  ((begin
     (define reverse-subtract
       (lambda (x y) (- y x)))
     (reverse-subtract 7 10)) => 3)
  ((begin
     (define add4
       (let ((x 4))
	 (lambda (y) (+ x y))))
     (add4 6)) => 10)
  (((lambda x x) 3 4 5 6) => (3 4 5 6))
  (((lambda (x y . z) z) 3 4 5 6) => (5 6))
  )

(define-tests conditionals-4.1.5
  ((if (> 3 2) 'yes 'no) => yes)
  ((if (> 2 3) 'yes 'no) => no)
  ((if (> 3 2) (- 3 2) (+ 3 2)) => 1)
)

(define-tests assignments-4.1.6
  ((begin
     (define x 2)
     (+ x 1)) => 3)
  ((begin
     (define x 2)
     (+ x 1)
     (set! x 4)
     (+ x 1)) => 5)
)

(define-tests conditionals-4.2.1
  ((cond
    ((> 3 2) 'greater)
    ((< 3 2) 'less)) => greater)
  ((cond
    ((> 3 3) 'greater)
    ((< 3 3) 'less)
    (else 'equal)) => equal)
  ((cond
    ((assv 'b '((a 1) (b 2))) => cadr)
    (else #f)) => 2)
  ((case (* 2 3)
     ((2 3 5 7) 'prime)
     ((1 4 6 8 9) 'composite)) => composite)
  ((case (car '(c d))
     ((a e i o u) 'vowel)
     ((w y) 'semivowel)
     (else 'consonant)) => consonant)
  ((and (= 2 2) (> 2 1)) => #t)
  ((and (= 2 2) (< 2 1)) => #f)
  ((and 1 2 'c '(f g)) => (f g))
   ((and) => #t)
   ((or (= 2 2) (> 2 1)) => #t)
   ((or (= 2 2) (< 2 1)) => #t)
   ((or #f #f #f) => #f)
   ((or (memq 'b '(a b c)) (/ 3 0)) => (b c))
)

(define-tests binding-constructs-4.2.2
  ((let ((x 2) (y 3)) (* x y)) => 6)
  ((let ((x 2) (y 3))
     (let ((x 7)
	   (z (+ x y)))
       (* z x))) => 35)
  ((let ((x 2) (y 3))
     (let* ((x 7)
	    (z (+ x y)))
       (* z x))) => 70)
  ((letrec
       ((even?
	 (lambda (n)
	   (if (zero? n)
	       #t
	       (odd? (- n 1)))))
	(odd?
	 (lambda (n)
	   (if (zero? n)
	       #f
	       (even? (- n 1))))))
     (even? 88)) => #t)
  )

(define-tests sequencing-4.2.3
  ((begin
     (define x 0)
     (begin
       (set! x 5)
       (+ x 1))) => 6)
)

(define-tests iteration-4.2.4
  ((do ((vec (make-vector 5))
	(i 0 (+ i 1)))
       ((= i 5) vec)
     (vector-set! vec i i)) => #(0 1 2 3 4))
  ((let ((x '(1 3 5 7 9)))
     (do ((x x (cdr x))
	  (sum 0 (+ sum (car x))))
	 ((null? x) sum))) => 25)
  ((let loop ((numbers '(3 -2 1 6 -5))
	      (nonneg '())
	      (neg '()))
     (cond
      ((null? numbers) (list nonneg neg))
      ((>= (car numbers) 0)
       (loop (cdr numbers)
	     (cons (car numbers) nonneg)
	     neg))
      ((< (car numbers) 0)
       (loop (cdr numbers)
	     nonneg
	     (cons (car numbers) neg)))))
   => ((6 1 3) (-5 -2)))
)

(define-tests quasiquotation-4.2.6
  (`(list ,(+ 1 2) 4)
   => (list 3 4))
  ((let ((name 'a)) `(list ,name ',name))
   => (list a (quote a)))
  (`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
   => (a 3 4 5 6 b))
  (`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
   => ((foo 7) . cons))
  (`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
   => #(10 5 2 4 3 8))
  (`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
   => (a `(b ,(+ 1 2) ,(foo 4 d) e) f))
  ((let ((name1 'x)
	 (name2 'y))
     `(a `(b ,,name1 ,',name2 d) e))
   => (a `(b ,x ,'y d) e))
  ((quasiquote (list (unquote (+ 1 2)) 4))
   => (list 3 4))
  ('(quasiquote (list (unquote (+ 1 2)) 4))
   => `(list ,(+ 1 2) 4))
)

(define-tests binding-constructs-for-syntactic-keywords-4.3.1
  ((let-syntax
       ((when (syntax-rules ()
		((when test stmt1 stmt2 ...)
		 (if test
		     (begin stmt1 stmt2 ...))))))
     (let ((if #t))
       (when if (set! if 'now))
       if))
   => now)
  ((let ((x 'outer))
     (let-syntax ((m (syntax-rules () ((m) x))))
       (let ((x 'inner))
	 (m))))
   => outer)
  ;; ((letrec-syntax
  ;;      ((my-or (syntax-rules ()
  ;; 		 ((my-or) #f)
  ;; 		 ((my-or e) e)
  ;; 		 ((my-or e1 e2 ...)
  ;; 		  (let ((temp e1))
  ;; 		    (if temp
  ;; 			temp
  ;; 			(my-or e2 ...)))))))
  ;;    (let ((x #f)
  ;; 	   (y 7)
  ;; 	   (temp 8)
  ;; 	   (let odd?)
  ;; 	   (if even?))
  ;;      (my-or x
  ;; 	      (let temp)
  ;; 	      (if y)
  ;; 	      y)))
  ;;  => 7)
)

(define-tests pattern-language-4.3.2
  ((let ((=> #f))
     (cond (#t => 'ok))) => ok)
)

(define-tests top-level-definitions-5.2.1
  ((begin
     (define add3 (lambda (x) (+ x 3)))
     (add3 3))
   => 6)
  ((begin
     (define first car)
     (first '(1 2)))
   => 1)
  )

(define-tests internal-definitions-5.2.2
  ((let ((x 5))
     (define foo (lambda (y) (bar x y)))
     (define bar (lambda (a b) (+ (* a b) a)))
     (foo (+ x 3))) => 45)
  ((let ((x 5))
     (letrec ((foo (lambda (y) (bar x y)))
	      (bar (lambda (a b) (+ (* a b) a))))
       (foo (+ x 3)))) => 45)
  )

(define-tests equivalence-predicates-6.1
  ((eqv? 'a 'a) => #t)
  ((eqv? 'a 'b) => #f)
  ((eqv? 2 2) => #t)
  ((eqv? '() '()) => #t)
  ((eqv? 100000000 100000000) => #t)
  ((eqv? (cons 1 2) (cons 1 2)) => #f)
  ((eqv? (lambda () 1) (lambda () 2)) => #f)
  ((eqv? #f 'nil) => #f)
  ((let ((p (lambda (x) x)))
     (eqv? p p)) => #t)
  ((begin
     (define gen-counter
       (lambda ()
	 (let ((n 0))
	   (lambda () (set! n (+ n 1)) n))))
     (let ((g (gen-counter)))
       (eqv? g g)))
   => #t)
  ((begin
     (define gen-counter
       (lambda ()
	 (let ((n 0))
	   (lambda () (set! n (+ n 1)) n))))
     (eqv? (gen-counter) (gen-counter)))
   => #f)
  ((letrec
       ((f (lambda () (if (eqv? f g) 'f 'both)))
	(g (lambda () (if (eqv? f g) 'g 'both))))
     (eqv? f g)) => #f)
  ((let ((x '(a))) (eqv? x x)) => #t)
  ((eq? 'a 'a) => #t)
  ((eq? (list 'a) (list 'a)) => #f)
  ((eq? '() '()) => #t)
  ((eq? car car) => #t)
  ((let ((x '(a))) (eq? x x)) => #t)
  ((let ((x '#())) (eq? x x)) => #t)
  ((let ((p (lambda (x) x))) (eq? p p)) => #t)
  ((equal? 'a 'a) => #t)
  ((equal? '(a) '(a)) => #t)
  ((equal? '(a (b) c) '(a (b) c)) => #t)
  ((equal? "abc" "abc") => #t)
  ((equal? 2 2) => #t)
  )

(define-tests numerical-operations-6.2.5
  ((max 3 4) => 4)
  ((max 3.9 4) => 4.0)
  ((- 3 4) => -1)
  ((- 3 4 5) => -6)
  ((- 3) => -3)
;((/ 3 4 5) => 3/20) ; hmm
;((/ 3) => 1/3)
  ((abs -7) => 7)
  ((modulo 13 4) => 1)
  ((remainder 13 4) => 1)
  ((modulo -13 4) => 3)
  ((remainder -13 4) => -1)
  ((modulo 13 -4) => -3)
  ((remainder 13 -4) => 1)
  ((modulo -13 -4) => -1)
  ((remainder -13 -4) => -1)
  ((remainder -13 -4.0) => -1.0)
  ((gcd 32 -36) => 4)
  ((gcd) => 0)
  ((lcm 32 -36) => 288)
  ((lcm 32.0 -36) => 288.0)
  ((lcm) => 1)
  ((floor -4.3) => -5.0)
  ((ceiling -4.3) => -4.0)
  ((truncate -4.3) => -4.0)
  ((round -4.3) => -4.0)
  ((floor 3.5) => 3.0)
  ((ceiling 3.5) => 4.0)
  ((truncate 3.5) => 3.0)
  ((round 3.5) => 4.0)
;((round 7/2) => 4)
  ((round 7) => 7)
  )

(define-tests booleans-6.3.1
  (#t => #t)
  (#f => #f)
  ('#f => #f)
  ((not #t) => #f)
  ((not 3) => #f)
  ((not (list 3)) => #f)
  ((not #f) => #t)
  ((not '()) => #f)
  ((not (list)) => #f)
  ((not 'nil) => #f)
  ((boolean? #f) => #t)
  ((boolean? 0) => #f)
  ((boolean? '()) => #f)
)

(define-tests pairs-and-lists-6.3.2
  ((begin
     (define x (list 'a 'b 'c))
     (define y x)
     y) => (a b c))
  ((begin
     (define x (list 'a 'b 'c))
     (define y x)
     (list? y)) => #t)
  ((begin
     (define x (list 'a 'b 'c))
     (set-cdr! x 4)
     x) => (a . 4))
  ((begin
     (define x (list 'a 'b 'c))
     (define y x)
     (set-cdr! x 4)
     (eqv? x y)) => #t)
  ((begin
     (define x (list 'a 'b 'c))
     (define y x)
     (set-cdr! x 4)
     y) => (a . 4))
  ((begin
     (define x (list 'a 'b 'c))
     (define y x)
     (set-cdr! x 4)
     (list? y)) => #f)
  ((begin
     (define x (list 'a 'b 'c))
     (set-cdr! x 4)
     (set-cdr! x x)
     (list? x)) => #f)
  ((pair? '(a . b)) => #t)
  ((pair? '(a b c)) => #t)
  ((pair? '()) => #f)
  ((pair? '#(a b)) => #f)
  ((cons 'a '()) => (a))
  ((cons '(a) '(b c d)) => ((a) b c d))
  ((cons "a" '(b c)) => ("a" b c))
  ((cons 'a 3) => (a . 3))
  ((cons '(a b) 'c) => ((a b) . c))
  ((car '(a b c)) => a)
  ((car '((a) b c d)) => (a))
  ((car '(1 . 2)) => 1)
  ((cdr '((a) b c d)) => (b c d))
  ((cdr '(1 . 2)) => 2)
  ((list? '(a b c)) => #t)
  ((list? '()) => #t)
  ((list? '(a . b)) => #f)
  ((let ((x (list 'a)))
     (set-cdr! x x)
     (list? x)) => #f)
  ((list 'a (+ 3 4) 'c) => (a 7 c))
  ((list) => ())
  ((length '(a b c)) => 3)
  ((length '(a (b) (c d e))) => 3)
  ((length '()) => 0)
  ((append '(x) '(y)) => (x y))
  ((append '(a) '(b c d)) => (a b c d))
  ((append '(a (b)) '((c))) => (a (b) (c)))
  ((append '(a b) '(c . d)) => (a b c . d))
  ((append '() 'a) => a)
  ((reverse '(a b c)) => (c b a))
  ((reverse '(a (b c) d (e (f)))) => ((e (f)) d (b c) a))
  ((list-ref '(a b c d) 2) => c)
  ((list-ref '(a b c d) (inexact->exact (round 1.8))) => c)
  ((memq 'a '(a b c)) => (a b c))
  ((memq 'b '(a b c)) => (b c))
  ((memq 'a '(b c d)) => #f)
  ((memq (list 'a) '(b (a) c)) => #f)
  ((member (list 'a) '(b (a) c)) => ((a) c))
  ((memv 101 '(100 101 102)) => (101 102))
  ((begin
     (define e '((a 1) (b 2) (c 3)))
     (assq 'a e)) => (a 1))
  ((begin
     (define e '((a 1) (b 2) (c 3)))
     (assq 'b e)) => (b 2))
  ((begin
     (define e '((a 1) (b 2) (c 3)))
     (assq 'd e)) => #f)
  ((assq (list 'a) '(((a)) ((b)) ((c)))) => #f)
  ((assoc (list 'a) '(((a)) ((b)) ((c)))) => ((a)))
  ((assv 5 '((2 3) (5 7) (11 13))) => (5 7))
)

(define-tests symbols-6.3.3
  ((symbol? 'foo) => #t)
  ((symbol? (car '(a b))) => #t)
  ((symbol? "bar") => #f)
  ((symbol? 'nil) => #t)
  ((symbol? '()) => #f)
  ((symbol? #f) => #f)
  ((symbol->string 'flying-fish) => "flying-fish")
  ((symbol->string 'Martin) => "martin")
  ((symbol->string
    (string->symbol "Malvina")) => "Malvina")
  ((eq? 'mISSISSIppi 'mississippi) => #t)
  ((string->symbol "mISSISSIppi") => mISSISSIppi)
  ((eq? 'bitBlt (string->symbol "bitBlt")) => #f)
  ((eq? 'JollyWog
	(string->symbol
	 (symbol->string 'JollyWog))) => #t)
  ((string=? "K. Harper, M.D."
	     (symbol->string
	      (string->symbol "K. Harper, M.D."))) => #t)
)

(define-tests characters-6.3.4
  ((char<? #\A #\B) => #t)
  ((char<? #\a #\b) => #t)
  ((char<? #\0 #\9) => #t)
  ((char-ci=? #\A #\a) => #t)
)

(define-tests vectors-6.3.6
  ((vector 'a 'b 'c) => #(a b c))
  ((vector-ref '#(1 1 2 3 5 8 13 21) 5) => 8)
  ((vector-ref '#(1 1 2 3 5 8 13 21)
	       (let ((i (round (* 2 (acos -1)))))
		 (if (inexact? i)
		     (inexact->exact i)
		     i))) => 13)
  ((let ((vec (vector 0 '(2 2 2 2) "Anna")))
     (vector-set! vec 1 '("Sue" "Sue"))
     vec) => #(0 ("Sue" "Sue") "Anna"))
  ((vector->list '#(dah dah didah)) => (dah dah didah))
  ((list->vector '(dididit dah)) => #(dididit dah))
)

(define-tests control-features-6.4
  ((procedure? car) => #t)
  ((procedure? 'car) => #f)
  ((procedure? (lambda (x) (* x x))) => #t)
  ((procedure? '(lambda (x) (* x x))) => #f)
  ((call-with-current-continuation procedure?) => #t)
  ((apply + (list 3 4)) => 7)
  ((begin
     (define compose
       (lambda (f g)
	 (lambda args
	   (f (apply g args)))))
     ((compose sqrt *) 12 75))
   => 30)
  ((map cadr '((a b) (d e) (g h))) => (b e h))
  ((map (lambda (n) (expt n n)) '(1 2 3 4 5))
   => (1 4 27 256 3125))
  ((map + '(1 2 3) '(4 5 6)) => (5 7 9))
  ((let ((v (make-vector 5)))
     (for-each (lambda (i)
		 (vector-set! v i (* i i)))
	       '(0 1 2 3 4)) v)
   => #(0 1 4 9 16))
  ((force (delay (+ 1 2))) => 3)
  ((let ((p (delay (+ 1 2))))
     (list (force p) (force p)))
   => (3 3))
  ((begin
     (define a-stream
       (letrec ((next
		 (lambda (n)
		   (cons n (delay (next (+ n 1)))))))
	 (next 0)))
     (define head car)
     (define tail
       (lambda (stream) (force (cdr stream))))
     (head (tail (tail a-stream))))
   => 2)
  ((begin
     (define count 0)
     (define p
       (delay (begin (set! count (+ count 1))
		     (if (> count x)
			 count
			 (force p)))))
     (define x 5)
     (force p))
   => 6)
  ((begin
     (define count 0)
     (define p
       (delay (begin (set! count (+ count 1))
		     (if (> count x)
			 count
			 (force p)))))
     (define x 5)
     (force p)
     (begin (set! x 10)
	    (force p)))
   => 6)
  ((call-with-current-continuation
    (lambda (exit)
      (for-each (lambda (x)
		  (if (negative? x)
		      (exit x)))
		'(54 0 37 -3 245 19)) #t))
   => -3)
  ((begin
     (define list-length
       (lambda (obj)
	 (call-with-current-continuation
	  (lambda (return)
	    (letrec ((r
		      (lambda (obj)
			(cond ((null? obj) 0)
			      ((pair? obj)
			       (+ (r (cdr obj)) 1))
			      (else (return #f))))))
	      (r obj))))))
     (list-length '(1 2 3 4))) => 4)
  ((begin
     (define list-length
       (lambda (obj)
	 (call-with-current-continuation
	  (lambda (return)
	    (letrec ((r
		      (lambda (obj)
			(cond ((null? obj) 0)
			      ((pair? obj)
			       (+ (r (cdr obj)) 1))
			      (else (return #f))))))
	      (r obj))))))
     (list-length '(a b . c))) => #f)
  ((call-with-values (lambda () (values 4 5))
     (lambda (a b) b)) => 5)
  ((call-with-values * -) => -1)
  ((let ((path '())
          (c #f))
      (let ((add (lambda (s)
                   (set! path (cons s path)))))
        (dynamic-wind
          (lambda () (add 'connect))
          (lambda ()
            (add (call-with-current-continuation
                   (lambda (c0)
		     (set! c c0)
                     'talk1))))
          (lambda () (add 'disconnect)))
        (if (< (length path) 4)
            (c 'talk2)
            (reverse path))))
   => (connect talk1 disconnect
	       connect talk2 disconnect))
)

(define-tests eval-6.5
  ((eval '(* 7 3) (scheme-report-environment 5)) => 21)
  ((let ((f (eval '(lambda (f x) (f x x))
		  (null-environment 5))))
     (f + 10)) => 20)
)

(define-tests runge-kutta-example
  ((begin
     (define integrate-system
       (lambda (system-derivative initial-state h)
	 (let ((next (runge-kutta-4 system-derivative h)))
	   (letrec ((states
		     (cons initial-state
			   (delay (map-streams next states)))))
	     states))))

     (define runge-kutta-4
       (lambda (f h)
	 (let ((*h (scale-vector h))
	       (*2 (scale-vector 2))
	       (*1/2 (scale-vector (/ 1 2)))
	       (*1/6 (scale-vector (/ 1 6))))
	   (lambda (y)
	     ;; y is a system state
	     (let* ((k0 (*h (f y)))
		    (k1 (*h (f (add-vectors y (*1/2 k0)))))
		    (k2 (*h (f (add-vectors y (*1/2 k1)))))
		    (k3 (*h (f (add-vectors y k2)))))
	       (add-vectors y
			    (*1/6 (add-vectors k0 (*2 k1) (*2 k2) k3))))))))

     (define elementwise
       (lambda (f)
	 (lambda vectors
	   (generate-vector
	    (vector-length (car vectors))
	    (lambda (i)
	      (apply f
		     (map (lambda (v) (vector-ref  v i))
			  vectors)))))))

     (define generate-vector
       (lambda (size proc)
	 (let ((ans (make-vector size)))
	   (letrec ((loop
		     (lambda (i)
		       (cond ((= i size) ans)
			     (else
			      (vector-set! ans i (proc i))
			      (loop (+ i 1)))))))
	     (loop 0)))))

     (define add-vectors (elementwise +))

     (define scale-vector
       (lambda (s)
	 (elementwise (lambda (x) (* x s)))))

     (define map-streams
       (lambda (f s)
	 (cons (f (head s))
	       (delay (map-streams f (tail s))))))

     (define head car)
     (define tail
       (lambda (stream) (force (cdr stream))))

     (define damped-oscillator
       (lambda (R L C)
	 (lambda (state)
	   (let ((Vc (vector-ref state 0))
		 (Il (vector-ref state 1)))
	     (vector (- 0 (+ (/ Vc (* R C)) (/ Il C)))
		     (/ Vc L))))))

     (define the-states
       (integrate-system
	(damped-oscillator 10000 1000 .001)
	'#(1 0)
	.01))

     ; todo bl: think of a better (= non-trivial) way to test this.
     ; Perhaps precompute some values and check the Scheme answer is
     ; within some tolerance?
     (let ((x (tail the-states)))
       (display x)
       (vector? (head x)))) => #t))

