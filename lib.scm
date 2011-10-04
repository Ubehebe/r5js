; verbatim from 7.3
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax let (syntax-rules ()
  ((let () body) 
   ((lambda () body)))
  ((let ((var expr)) body) 
   ((lambda (var) body) expr))
  ((let ((var1 expr1) (var2 expr2) ...) body)
   ((lambda (var1) (let ((var2 expr2) ...) body)) expr1))))