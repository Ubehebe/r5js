;; This file contains the unit testing framework, not the tests themselves.
;; The framework provides a simple syntax for specifying the expected value
;; of an expression, as well as simple human- and machine-readable outputs
;; for success and failure. Example:
;;
;; (define-tests foo-tests
;;   ((+ 1 2) => 3)
;;   ((* 10 10) => 100)
;;   ((+ 1 1) => 3))
;;
;; should produce output like this:
;;
;; (fail foo-tests (input (+ 1 1)) (want 3) (got 2))
;; (foo-tests (3 tests) (1 failed))

(define-syntax define-tests
  (syntax-rules (=>)
    ((define-tests name (input => output) ...)
     (let* ((result
	     (foldl
	      (run-test-group (quote name))
	      (cons 0 0)
	      (list (quote (input . output)) ...)))
	    (num-errors (car result))
	    (num-tests (cdr result)))
       (write (list
	       'name
	       (list num-tests 'tests)
	       (list num-errors 'failed)))))))

(define (run-test-group group-name)
  (lambda (l r)
    (let* ((cur-errors (car l))
	   (cur-total (cdr l))
	   (input (car r))
	   (expected-output (cdr r))
	   (actual-output
	    (eval input (scheme-report-environment 5))))
      (if (equal? actual-output expected-output)
	  (cons cur-errors (+ cur-total 1))
	  (begin
	    (write (list
		    'fail
		    group-name
		    (list 'input input)
		    (list 'want expected-output)
		    (list 'got actual-output)))
	    (cons (+ cur-errors 1) (+ cur-total 1)))))))

; todo bl: this file won't work unless the Scheme implementation has
; the non-standard procedure will-eval?. Document.
(define-syntax define-negative-tests
  (syntax-rules ()
    ((define-negative-tests name input ...)
     (let* ((result
	     (foldl
	      (run-negative-test-group (quote name))
	      (cons 0 0)
	      (quote ((quote input) ...))))
	    (num-errors (car result))
	    (num-tests (cdr result)))
       (write (list
	       'name
	       (list num-tests 'tests)
	       (list num-errors 'failed)))))))

(define (run-negative-test-group group-name)
  (lambda (l r)
    (let* ((cur-errors (car l))
	   (cur-total (cdr l))
	   (input (car r)))
      (if (not (will-eval? input (scheme-report-environment 5)))
	  (cons cur-errors (+ cur-total 1))
	  (begin
	    (write (list
		    'fail
		    group-name
		    (list 'input input)
		    '(want eval-failure)
		    (list 'got actual-output)))
	    (cons (+ cur-errors 1) (+ cur-total 1)))))))
