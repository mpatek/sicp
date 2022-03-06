(define (in-list? x ls)
  (cond
    ((null? ls) false)
    ((eq? x (car ls)) true)
    (else (in-list? x (cdr ls)))))

(define (count-pairs x)
  (define seen '())
  (define (count-pairs-inner x)
    (cond
      ((not (pair? x)) 0)
      ((in-list? x seen) 0)
      (else
	(set! seen (cons x seen))
	(+ 
	  (count-pairs-inner (car x))
	  (count-pairs-inner (cdr x))
	  1))))
  (count-pairs-inner x))

(define x1 (list 'a 'b 'c))
(eq? 3 (count-pairs x1))

(define x2-1 (list 'a 'b))
(define x2-2 (cons x2-1 (cdr x2-1)))
(eq? 3 (count-pairs x2-2))

(define x3-1 (list 'a))
(define x3-2 (cons x3-1 x3-1))
(define x3-3 (cons x3-2 x3-2))
(eq? 3 (count-pairs x3-3))
