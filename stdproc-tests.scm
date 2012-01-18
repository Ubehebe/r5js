; This file contains all examples from R5RS.
; For examples that should not evaluate, the second half of the datum should
; be 'error; for examples that should evaluate, but whose value is unspecified,
; the second half of the datum should be 'unspecified.

(define eqv?-examples ; p. 18
  '(
    ((eqv? 'a 'a) . #t)
    ((eqv? 'a 'b) . #f)
    ((eqv? 2 2) . #t)
    ((eqv? '() '()) . #t)
    ((eqv? 100000000 1000000000) . #t)
    ((eqv? (cons 1 2) (cons 1 2)) . #f)
    ((eqv? (lambda () 1) (lambda () 2)) . #f)
    ((eqv? #f 'nil) . #f)
    ((let ((p (lambda (x) x))) (eqv? p p)) . #t)
    ((eqv? "" "") . 'unspecified)
    ((eqv? '#() '#()) . 'unspecified)
    ((eqv? (lambda (x) x) (lambda (x) x)) . 'unspecified)
    ((eqv? (lambda (x) x) (lambda (y) y)) . 'unspecified)))


(define integer-division-examples ; p. 22
  '(
    ((modulo 13 4) . 1)
    ((remainder 13 4) . 1)
    ((modulo -13 4) . 3)
    ((remainder -13 4) . -1)
    ((modulo 13 -4) . -3)
    ((remainder 13 -4) . 1)
    ((modulo -13 -4) . -1)
    ((remainder -13 -4) . -1)
    ((= 51 (+ (* 8 (quotient 51 8)) (remainder 51 8))) . #t)))

(define not-examples ; p. 25
  '(
    ((not #t) . #f)
    ((not 3) . #f)
    ((not (list 3)) . #f)
    ((not #f) . #t)
    ((not '()) . #f)
    ((not (list)) . #f)
    ((not 'nil) . #f)))

(run-tests integer-division-examples)
(display 'done)
(run-tests not-examples)
