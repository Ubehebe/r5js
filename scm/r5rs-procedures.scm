; Copyright 2011-2014 Brendan Linn
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; This file contains definitions for almost all the procedures in chapter 6
; of R5RS listed as "library procedures".
;
; In a few cases, I have chosen to implement library procedures
; as primitive procedures. These decisions are explained in comments
; in this file.
;
; This file also contains some helper functions that were useful in
; implementing the library procedures.

(define (equal? x y) ; p. 19
  (cond
   ((and (pair? x) (pair? y))
    (and (equal? (car x) (car y))
	 (equal? (cdr x) (cdr y))))
   ((and (vector? x) (vector? y))
    (equal? (vector->list x) (vector->list y)))
   ((and (string? x) (string? y))
    (equal? (string->list x) (string->list y)))
   (else (eqv? x y))))

(define (zero? z) (= z 0)) ; p. 22

(define (positive? x) (> x 0)) ; p. 22

(define (negative? x) (< x 0)) ; p. 22

(define (odd? n) (= 1 (remainder n 2))) ; p. 22

(define (even? n) (= 0 (remainder n 2))) ; p. 22

(define (max x . ys) ; p. 22
  (if (null? ys)
      x
      (foldl
       (lambda (y z) (if (< y z) z y))
       x
       ys)))

(define (min x . ys) ; p. 22
  (if (null? ys)
      x
      (foldl
       (lambda (y z) (if (< y z) y z))
       x
       ys)))

(define (abs x) (if (> x 0) x (- x))) ; p. 22

(define (euclid p q) ; helper, not in R5RS
  (cond
   ((= q 0) p)
   ((< p q) (euclid q p))
   (else (euclid q (modulo p q)))))

(define (gcd . ns) ; p. 23
  (foldl (lambda (l r) (euclid l (abs r))) 0 ns))

(define (lcm2 p q) ; helper, not in R5RS
  (/ (* p q) (euclid p q)))

(define (lcm . ns) ; p. 23
  (foldl (lambda (l r) (lcm2 l (abs r))) 1 ns))

(define (not p) (if p #f #t)) ; p. 25

; Note: boolean? is listed as a library procedure, and can
; easily be implemented as
; (define (boolean? p)
;   (or (eqv? p #t) (eqv? p #f)))
; In this implementation, all of the basic type predicates
; are primitive procedures because they are used to do
; rudimentary ("primitive"?) typechecking on the arguments to
; primitive procedures.

(define (foldr f start xs) ; helper function, not in R5RS
  (if (null? xs)
      start
      (f (car xs) (foldr f start (cdr xs)))))

(define (compose . fs) ; helper function, not in R5RS
  (foldr
   (lambda (l r) (lambda (x) (l (r x))))
   (lambda (y) y)
   fs))

(define caar (compose car car)) ; p. 26
(define cadr (compose car cdr))
(define cdar (compose cdr car))
(define cddr (compose cdr cdr))

(define caaar (compose car car car)) ; p. 26
(define caadr (compose car car cdr))
(define cadar (compose car cdr car))
(define caddr (compose car cdr cdr))
(define cdaar (compose cdr car car))
(define cdadr (compose cdr car cdr))
(define cddar (compose cdr cdr car))
(define cdddr (compose cdr cdr cdr))

(define caaaar (compose car car car car)) ; p. 26
(define caaadr (compose car car car cdr))
(define caadar (compose car car cdr car))
(define caaddr (compose car car cdr cdr))
(define cadaar (compose car cdr car car))
(define cadadr (compose car cdr car cdr))
(define caddar (compose car cdr cdr car))
(define cadddr (compose car cdr cdr cdr))
(define cdaaar (compose cdr car car car))
(define cdaadr (compose cdr car car cdr))
(define cdadar (compose cdr car cdr car))
(define cdaddr (compose cdr car cdr cdr))
(define cddaar (compose cdr cdr car car))
(define cddadr (compose cdr cdr car cdr))
(define cdddar (compose cdr cdr cdr car))
(define cddddr (compose cdr cdr cdr cdr))

(define (list? l)
  ; The interview staple: find a cycle in a linked list.
  ;
  ; The main idea is to send one pointer through the list hitting every node,
  ; and another pointer through the list hitting every second node. If there
  ; is a cycle of length n, the two pointers will eventually coincide.
  ;
  ; Proof: suppose pointers X and Y start at positions x and y
  ; respectively on the cycle, 0 <= x,y < n. After k moves:
  ;
  ; X is at x+k (mod n)
  ; Y is at y+2k (mod n)
  ;
  ; So we just have to show that
  ;
  ; x+k = y+2k (mod n)
  ;
  ; always has a solution. Sure:
  ;
  ; k = x-y (mod n)
  ;
  ; so the two pointers coincide after fewer than n steps.
  (define (detect-cycle x y)
    (cond ((null? x) (list? y)) ; x finished first, just check y is ok
	  ((null? y) (list? x)) ; y finished first, just check x is ok
	  ((or (not (pair? x)) (not (pair? y)))
	   #f) ; x or y is not a pair: fail
	  ((eq? x y) #f) ; cycle: fail
	  (else
	   (let* ((next-x (cdr x))
		  (next-y (cdr y)))
	     (cond ((null? next-y) (list? next-x)) ; y finished, just check x is ok
		   ((not (pair? next-y)) #f) ; next-y is not a pair: fail
		   (else ; advance x by 1 and y by 2
		    (detect-cycle next-x (cdr next-y))))))))
  (cond ((null? l) #t) ; various corner cases before input to detect-cycle
	((not (pair? l)) #f)
	((null? (cdr l))
	 (if (pair? (car l)) ; not sure about this but seems to be required...
	     (detect-cycle (car l) (caar l))
	     #t))
	((not (pair? (cdr l))) #f)
	(else (detect-cycle l (cdr l)))))

(define (list . xs) xs) ; p. 27

(define (foldl f start xs) ; helper function, not in R5RS
  (if (null? xs)
      start
      (foldl f (f start (car xs)) (cdr xs))))

(define (length xs) ; p. 27
  (foldl
   (lambda (x y) (+ x 1))
   0
   xs))

(define (append . xs) ; p. 27
  (define (append2 xs ys)
    (if (null? xs)
	ys
	(cons (car xs) (append2 (cdr xs) ys))))
  (foldl append2 '() xs))

(define (flip f) (lambda (x y) (f y x))) ; helper function, not in R5RS

(define (reverse xs) (foldl (flip cons) '() xs)) ; p. 27

(define (list-tail xs k) ; p. 27
  (if (= k 0)
      xs
      (list-tail (cdr xs) (- k 1))))

(define (list-ref xs k) (car (list-tail xs k))) ; p. 27

(define (member-abstract are-equal?) ; helper function, not in R5RS
  (lambda (x ys)
    (if (null? ys)
	#f
	(if (are-equal? x (car ys))
	    ys
	    ((member-abstract are-equal?) x (cdr ys))))))

(define memq (member-abstract eq?)) ; p. 27
(define memv (member-abstract eqv?)) ; p. 27
(define member (member-abstract equal?)) ; p. 27

(define (find xs predicate?) ; helper function, not in R5RS
  (cond
   ((null? xs) #f)
   ((predicate? (car xs)) (car xs))
   (else (find (cdr xs) predicate?))))

(define (assq obj alist) ; p. 27
  (find
   alist
   (lambda (pair) (eq? (car pair) obj))))

(define (assv obj alist) ; p. 27
  (find
   alist
   (lambda (pair) (eqv? (car pair) obj))))

(define (assoc obj alist) ; p. 27
  (find
   alist
   (lambda (pair) (equal? (car pair) obj))))

; These two are listed as library procedures, but I don't see how
; to implement them in Scheme without having a big switch on each character,
; and I'm not going to do that.
; (define char-upcase [primitive]) ; p. 29
; (define char-downcase [primitive]) ; p. 29

(define (char-ci-abstract pred?)
  (lambda (c1 c2)
    (pred? (char-downcase c1)
	   (char-downcase c2))))

(define char-ci=? (char-ci-abstract char=?)) ; p. 29
(define char-ci<? (char-ci-abstract char<?)) ; p. 29
(define char-ci>? (char-ci-abstract char>?)) ; p. 29
(define char-ci<=? (char-ci-abstract char<=?)) ; p. 29
(define char-ci>=? (char-ci-abstract char>=?)) ; p. 29

(define (string . cs) ; p. 30
  (let ((new-string (make-string (length cs))))
    (foldl
     (lambda (l r) (string-set! new-string l r) (+ l 1))
     0
     cs)
    new-string))

(define (lexicographic true-if goto-next-if compare-lengths-at-end) ; helper function, not in R5RS
  (lambda (str1 str2)
    (let* ((len1 (string-length str1))
	   (len2 (string-length str2))
	   (len (min len1 len2)))
      (define (lexico-tail i)
	(if (= i len)
	    (compare-lengths-at-end len1 len2)
	    (let* ((yes
		    (true-if (string-ref str1 i)
			     (string-ref str2 i)))
		   (maybe
		    (goto-next-if (string-ref str1 i)
				  (string-ref str2 i))))
	      (cond
	       (yes #t)
	       (maybe (lexico-tail (+ i 1)))
	       (else #f)))))
      (lexico-tail 0))))

(define string=?     (lexicographic (lambda (x y) #f) char=? =)) ; p. 30
(define string-ci=?  (lexicographic (lambda (x y) #f) char-ci=? =))
(define string<?     (lexicographic char<? char=? <))
(define string>?     (lexicographic char>? char=? >))
(define string<=?    (lexicographic char<? char=? <=))
(define string>=?    (lexicographic char>? char=? >=))
(define string-ci<?  (lexicographic char-ci<? char-ci=? <))
(define string-ci>?  (lexicographic char-ci>? char-ci=? >))
(define string-ci<=? (lexicographic char-ci<? char-ci=? <=))
(define string-ci>=? (lexicographic char-ci>? char-ci=? >=))

(define (substring str start end) ; p. 30
  (letrec ((new-str (make-string (- end start)))
	 (substr-tail
	  (lambda (i)
	    (string-set! new-str (- i start) (string-ref str i))
	    (if (= i start)
		new-str
		(substr-tail (- i 1))))))
    (if (= start end)
	new-str
	(substr-tail (- end 1)))))

(define (string-append . strs) ; p. 30
  (define (string-append2 str1 str2)
    (let* ((len1 (string-length str1))
	   (len2 (string-length str2))
	   (new-str (make-string (+ len1 len2))))
      (do
	  ((i 0 (+ i 1)))
	  ((= i len1) new-str)
	(string-set! new-str i (string-ref str1 i)))
      (do
	  ((i 0 (+ i 1)))
	  ((= i len2) new-str)
	(string-set! new-str (+ len1 i) (string-ref str2 i)))))
  (foldl string-append2 "" strs))

(define (string->list str) ; p. 30
  (let ((new-list '())
	(len (string-length str)))
    (do
	((i 0 (+ i 1)))
	((= i len) new-list)
      (set! new-list
	    (cons (string-ref str (- len i 1))
		  new-list)))))

(define (list->string cs) ; p. 30
  (let ((new-str
	 (make-string (length cs))))
  (foldl
   (lambda (l r)
     (string-set! new-str l r)
     (+ l 1))
   0
   cs)
  new-str))

(define (string-copy str) ; p. 30
  (let* ((len (string-length str))
	 (new-str (make-string len)))
    (do
	((i 0 (+ i 1)))
	((= i len) new-str)
      (string-set! new-str i (string-ref str i)))))

(define (string-fill! str c) ; p. 31
  (do
      ((i 0 (+ i 1)))
      ((= i (string-length str)) (if #f #f)) ; unspecified value
    (string-set! str i c)))

(define (list->vector xs) ; p. 31
  (let ((v (make-vector (length xs))))
    (foldl
     (lambda (l r)
       (vector-set! v l r)
       (+ l 1))
     0
     xs)
    v))

(define (vector . xs) (list->vector xs)) ; p. 31

(define (vector->list v) ; p. 31
  (let* ((new-list '())
	 (len (vector-length v)))
    (do
	((i 0 (+ i 1)))
	((= i len) new-list)
      (set! new-list
	    (cons (vector-ref v (- len i 1))
		  new-list)))))

(define (vector-fill! v f) ; p. 31
  (do
      ((i 0 (+ i 1)))
      ((= i (vector-length v)) (if #f #f)) ; unspecified value
    (vector-set! v i f)))

(define (transpose list-of-lists) ; helper proc, not in R5RS
  (cond
   ((null? list-of-lists) '())
   ((null? (car list-of-lists)) '())
   (else
    (let* ((firsts (map car list-of-lists))
	   (rests  (map cdr list-of-lists)))
      (cons firsts (transpose rests))))))

(define (map f l . ls) ; p. 32
  (define (map-common f xs)
    (foldr
     (lambda (l r) (cons (f l) r))
     '()
     xs))
  (if (null? ls)
      (map-common f l)
      (let ((list-of-lists (cons l ls))
	    (f-apply (lambda (list) (apply f list))))
	(map-common
	 f-apply
	 (transpose list-of-lists)))))

(define (for-each f l . ls) ; p. 32
  (define (for-each-common f xs)
    (foldl
     (lambda (l r) (f r))
     #f
     xs)
    (if #f #f)) ; unspecified return value
  (if (null? ls)
      (for-each-common f l)
      (let ((list-of-lists (cons l ls))
	    (f-apply (lambda (list) (apply f list))))
	(for-each-common
	 f-apply
	 (transpose list-of-lists)))))

(define (force object) (object)) ; verbatim from p. 32

(define make-promise ; verbatim from p. 33
  (lambda (proc)
    (let ((result-ready? #f)
	  (result #f))
      (lambda ()
	(if result-ready?
	    result
	    (let ((x (proc)))
	      (if result-ready?
		  result
		  (begin (set! result-ready? #t)
			 (set! result x)
			 result))))))))

(define (call-with-input-file string proc) ; p. 35
  (let* ((input (open-input-file string))
	 (result (proc input)))
    (close-input-port input)
    result))

(define (call-with-output-file string proc) ; p. 35
  (let* ((output (open-output-file string))
	 (result (proc output)))
    (close-output-port output)
    result))

(define (file->string filename) ; not in R5RS, thought it was useful
  (define (port->string port chars)
    (let ((maybe (read-char port)))
      (if (eof-object? maybe)
	  (list->string (reverse chars))
	  (port->string port (cons maybe chars)))))
  (let* ((input (open-input-file filename))
	 (result (port->string input '())))
    (close-input-port input)
    result))

(define (newline . maybe-port) ; p. 37
  (if (null? maybe-port)
      (write-char #\newline)
      (write-char #\newline (car maybe-port))))

(define oo===D ; Turing's normal-order Y combinator (not in R5RS)
  (lambda (g)
    (g (delay (oo===D g)))))