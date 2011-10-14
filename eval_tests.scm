(define (input-format-ok? input)
  (and (list? input) (= 2 (length input))))

(define (run-eval-tests ts)
  (letrec ((run-eval-tests-counter
	    (lambda (ts num)
	      (if (null? ts)
		  num
		  (let ((test (car ts))
			(rest (cdr ts)))
		    (if (not (input-format-ok? test))
			(list 'invalid 'test 'format test)
			(let* ((input (car test))
			       (expected (cadr test))
			       (actual (eval input (scheme-report-environment 5))))
			  (if (eq? expected actual)
			      (run-eval-tests-counter rest (+ 1 num))
			      (list 'error 'in test
				    'expected: expected
				    'actual: actual)))))))))
    (run-eval-tests-counter ts 0)))

(define sanity-checks
  (list
   '(#t #t)
   '(#f #f)
   '((+ 1 1) 2)))

(define macro-literal-matching-tests
  (list
   '(((lambda (x)
	(let-syntax
	    ((foo (syntax-rules (x)
		    ((foo x) 'literal)
		    ((foo y) 'nonliteral))))
	  (foo x)))
      'hello)
     literal) ; since x has the same binding in def and exp

   '(((lambda (x)
	(let-syntax
	    ((foo (syntax-rules (x)
		    ((foo x) 'literal)
		    ((foo y) 'nonliteral))))
	  ((lambda (x) (foo x)) 'hello)))
      'world)
     nonliteral) ; since x has different bindings in def and exp

   '((let-syntax
	 ((foo (syntax-rules (x)
		 ((foo x) 'literal)
		 ((foo y) 'nonliteral))))
       (foo x))
     literal) ; since x is unbound in both def and exp

   '((let-syntax
	 ((foo (syntax-rules (x)
		 ((foo x) 'literal)
		 ((foo y) 'nonliteral))))
       ((lambda (x) (foo x)) 'hello))
     nonliteral) ; since x is bound in exp but not in def
   )
  )

(define-syntax named-list
  (syntax-rules ()
    ((named-list) '())
    ((named-list x y ...) (cons (list 'x x) (named-list y ...)))))

(define all-tests
  (named-list
   sanity-checks
   macro-literal-matching-tests
   )
  )

(define (run-all-tests)
  (letrec ((run-all-tests-internal
	    (lambda (all-tests num)
	      (if (null? all-tests)
		  (list 'passed num 'tests)
		  (let* ((cur-test-set (car all-tests))
			 (cur-test-name (car cur-test-set))
			 (cur-tests (cadr cur-test-set))
			 (remaining (cdr all-tests))
			 (result (run-eval-tests cur-tests)))
		    (if (number? result)
			(run-all-tests-internal remaining (+ num result))
			(list 'error 'in cur-test-name result)))))))
    (run-all-tests-internal all-tests 0)))