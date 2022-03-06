(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define x1 (list 'a 'b 'c))
(eq? 3 (count-pairs x1))

(define x2-1 (list 'a 'b))
(define x2-2 (cons x2-1 (cdr x2-1)))
(eq? 4 (count-pairs x2-2))

(define x3-1 (list 'a))
(define x3-2 (cons x3-1 x3-1))
(define x3-3 (cons x3-2 x3-2))
(eq? 7 (count-pairs x3-3))

(define z (list 'a 'b 'c))
(set-cdr! (cdr (cdr z)) z)
; (count-pairs z) results in maximum recursion depth error
