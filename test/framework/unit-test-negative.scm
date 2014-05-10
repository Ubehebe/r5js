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
	       (list num-errors 'errors)))))))

(define (run-negative-test-group group-name)
  (lambda (l r)
    (let* ((cur-errors (car l))
	   (cur-total (cdr l))
	   (input (car r)))
      (if (not (will-eval? input (scheme-report-environment 5)))
	  (cons cur-errors (+ cur-total 1))
	  (begin
	    (display "error in test ")
	    (display group-name)
	    (display ": input ")
	    (display input)
	    (display ", expected failure, got success") ; todo bl fix display bugs
	    (display #\newline)
	    (cons (+ cur-errors 1) (+ cur-total 1)))))))