(define (list-in-list? ls1 ls2)
  (cond
    ((null? ls2) false)
    ((eq? ls1 ls2) true)
    (else (list-in-list? ls1 (cdr ls2)))))

(define (contains-cycle? x)
  (cond
    ((null? x) false)
    ((list-in-list? x (cdr x)) true)
    (else (contains-cycle? (cdr x)))))

(define x1 (list 'a 'b 'c))
(contains-cycle? x1)

(define x2-1 (list 'a 'b))
(define x2-2 (cons x2-1 (cdr x2-1)))
(contains-cycle? x2-2)

(define x3-1 (list 'a))
(define x3-2 (cons x3-1 x3-1))
(define x3-3 (cons x3-2 x3-2))
(contains-cycle? x3-3)

(define z (list 'a 'b 'c))
(set-cdr! (cdr (cdr z)) z)
(contains-cycle? z)
