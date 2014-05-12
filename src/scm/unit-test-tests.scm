(define-tests unit-test-framework-tests
  ((+ 1 1) => 3) ; to test that errors are reported correctly
  ((boolean? boolean?) => #f)
  ((boolean? #t) => #t))
