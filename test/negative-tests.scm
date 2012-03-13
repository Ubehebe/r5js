(define-negative-tests sanity-check-errors
  ()
  (+ "hello" "world")
  (1 1)
  (1 . 1)
  (cdr (x . y))
  (cadr '((1 2) . 3))
  let
  `(1 2 ,@(+ 3 4))
  (let ())
  (let (x ()) 1)
  (display "hello" (current-input-port))
  (close-input-port (current-output-port))
  (close-output-port (current-input-port))
)

(define-negative-tests r5rs-errors
  (car '()) ; p. 26
  (cdr '()) ; p. 26
  (begin
    (define (f) (list 'not-a-constant-list))
    (define (g) '(constant-list))
    (set-car! (g) 3)) ; p. 26
  (begin
    (define (f) (make-string 3 #\*))
    (define (g) "***")
    (string-set! (g) 0 #\?)) ; p. 30
  (string-set! (symbol->string 'immutable) 0 #\?) ; p. 30
  )

(define-negative-tests macro-errors
  (begin
    (define-syntax x
      (syntax-rules ()
	((x) 'macro)))
    (define x 'procedure-call)
    (x))
  (let-syntax
      ((foo
	(syntax-rules ()
	  ((foo x) x))))
    (foo (1 2)))
  (begin
    (define-syntax foo
      (syntax-rules (x)
	((foo x) 'literal-id)))
    (define (bar x) (foo x))
    (bar 32))
  (let-syntax
      ((foo
	(syntax-rules ()
	  ((foo (x . y)) y))))
    (foo (1 2)))
  (begin
    (define-syntax foo
      (syntax-rules ()
	((foo (x)) x)))
    (foo 2))
  (let-syntax
      ((foo
	(syntax-rules ()
	  ((foo (x y ...) (z w ...))
	   (+ y ... w ...)))))
    (foo () ()))
  (begin
    (define-syntax foo
      (syntax-rules ()
	((foo) x)))
    ((lambda (x) (foo)) 2))
  (let-syntax
      ((foo
	(syntax-rules ()
	  ((foo (x ...)) 'ok))))
    (foo (1 . 2)))
  (begin
    (define-syntax foo
      (syntax-rules ()
	((foo (x ...)) 'ok)))
    (foo #()))
  (let-syntax
      ((foo
	(syntax-rules ()
	  ((foo #(x ...)) 'ok))))
    (foo ()))
  (begin
    (define-syntax foo
      (syntax-rules ()
	((foo (x . y)) 'ok)))
    (foo #()))
)

(define-negative-tests control-feature-errors
  (apply apply '(+ (3 4 5)))
  (eval '+ (null-environment 5))
  (eval () (null-environment 5))
  (eval '() (null-envrionment 5))
  (eval '(()) (null-environment 5))
  (eval (()) (null-environment 5))
)

(define-negative-tests syntax-rebinding-errors
  (define x let)
  (define x define)
  (let ((x let*)) 1)
  (let ((x let)) 1)
)

(define-negative-tests mutation-errors
  (begin
    (define x "hello")
    (string-set! x 0 #\x))
  (let ((x '(1 2 3)))
    (set-car! x 'hello))
  (begin
    (define x '(1 2 3))
    (set-cdr! x 'hello))
  (let ((x '(1 . 2)))
    (set-car! x 'hello))
  (begin
    (define x '#(a b c))
    (vector-set! x 2 "hi"))
)