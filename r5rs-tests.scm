; This file lists every example in the R5RS standard that should evaluate
; successfully. For those examples that have an unspecified value, we just
; want to check that they evaluate to *something*. Their value is listed here
; as the symbol 'unspecified.

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
;; ;  ((case (car '(c d))
;; ;     ((a) 'a)
;; ;     ((b) 'b)) => unspecified)
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
  ;; ((begin
  ;;    (display "4 plus 1 equals ")
  ;;    (display (+ 4 1))) => 'unspecified)
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

;; (define-tests quasiquotation-4.2.6
;;   (`(list ,(+ 1 2) 4)
;;    => (list 3 4))
;;   ((let ((name 'a)) `(list ,name ',name))
;;    => (list a (quote a)))
;;   (`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
;;    => (a 3 4 5 6 b))
;;   (`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
;;    => ((foo 7) . cons))
;;   (`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
;;    => #(10 5 2 4 3 8))
;;   (`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
;;    => (a `(b ,(+ 1 2) ,(foo 4 d) e) f))
;;   ((let ((name1 'x)
;; 	 (name2 'y))
;;      `(a `(b ,,name1 ,',name2 d) e))
;;    => (a `(b ,x ,'y d) e))
;;   ((quasiquote (list (unquote (+ 1 2)) 4))
;;    => (list 3 4))
;;   ('(quasiquote (list (unquote (+ 1 2)) 4))
;;    => `(list ,(+ 1 2) 4))
;; )

;; (define-tests binding-constructs-for-syntactic-keywords-4.3.1
;;   ((let-syntax
;;        ((when (syntax-rules ()
;; 		((when test stmt1 stmt2 ...)
;; 		 (if test
;; 		     (begin stmt1 stmt2 ...))))))
;;      (let ((if #t))
;;        (when if (set! if 'now))
;;        if))
;;    => now)
;;   ((let ((x 'outer))
;;      (let-syntax ((m (syntax-rules () ((m) x))))
;;        (let ((x 'inner))
;; 	 (m))))
;;    => outer)
;;   ((letrec-syntax
;;        ((my-or (syntax-rules ()
;; 		 ((my-or) #f)
;; 		 ((my-or e) e)
;; 		 ((my-or e1 e2 ...)
;; 		  (let ((temp e1))
;; 		    (if temp
;; 			temp
;; 			(my-or e2 ...)))))))
;;      (let ((x #f)
;; 	   (y 7)
;; 	   (temp 8)
;; 	   (let odd?)
;; 	   (if even?))
;;        (my-or x
;; 	      (let temp)
;; 	      (if y)
;; 	      y)))
;;    => 7)
;; )

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
)

(define-tests equivalence-predicates-6.1
  ((eqv? 'a 'a) => #t)
  ((eqv? 'a 'b) => #f)
  ((eqv? 2 2) => #t)
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
;  ((gcd 32 -36) => 4)
;  ((gcd) => 0)
;  ((lcm 32 -36) => 288)
;  ((lcm 32.0 -36) => 288.0)
;  ((lcm) => 1)
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