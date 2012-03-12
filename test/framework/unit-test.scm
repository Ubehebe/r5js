;; This file contains the unit testing framework (not the tests themselves).

;; This is a fairly natural syntax for unit tests. Example:
;;
;; (define-tests sanity-checks
;;   ((+ 1 2) => 3)
;;   ((* 10 10) => 100)
;;   ((begin
;;      (define x 3)
;;      x) => 3))
;;
;; The only limitation is that the input has to be a single datum.
;; If you have more than one datum to evaluate, you have to wrap them all
;; in a (begin ...). This is due to a limitation of the macro pattern
;; language syntax: we would like to write a pattern like
;;
;; (define-tests name (input ... => output) ...)
;;
;; but according to R5RS section 4.3.2, ellipses in pattern datums must
;; always be immediately followed by a right paren. So the first ellipsis
;; in our pattern is illegal. (Implementations like PLT Scheme seem to
;; support them, but I don't.)
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
       (display (quote name))
       (display ": ")
       (display num-tests)
       (display " tests, ")
       (display num-errors)
       (display " errors")
       (display #\newline)))))

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
	    (display "error in test ")
	    (display group-name)
	    (display ": input ")
	    (display input)
	    (display ", expected ")
	    (display expected-output)
	    (display ", got ")
	    (display actual-output)
	    (display #\newline)
	    (cons (+ cur-errors 1) (+ cur-total 1)))))))