; verbatim from 7.3
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

; 7.3 uses a let to avoid evaluating test1 twice in the last clause
(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...) (if test1 test1 (or test2 ...)))))