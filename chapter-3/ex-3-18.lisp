(define (in-list? x ls)
  (cond
    ((null? ls) false)
    ((eq? x (car ls)) true)
    (else (in-list? x (cdr ls)))))

(define (contains-cycle? x)
  (define seen '())
  (define (contains-cycle-inner x)
    (cond
      ((null? x) false)
      ((in-list? x seen) true)
      (else
	(set! seen (cons x seen))
	(contains-cycle-inner (cdr x)))))
  (contains-cycle-inner x))

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
