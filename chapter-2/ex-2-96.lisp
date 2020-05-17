(define (=zero? x) (= 0 x))
(define (mul x y) (* x y))
(define (add x y) (+ x y))
(define (div x y) (/ x y))

(define (make-polynomial variable term-list) (list variable term-list))

(define (variable p) (car p))
(define (term-list p) (cadr p))
(define (same-variable? x y) (eq? x y))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
	((empty-termlist? L2) L1)
	(else
	  (let ((t1 (first-term L1)) (t2 (first-term L2)))
	    (cond ((> (order t1) (order t2))
		   (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		  ((< (order t1) (order t2))
		   (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		  (else
		    (adjoin-term
		      (make-term (order t1)
				 (add (coeff t1) (coeff t2)))
		      (add-terms (rest-terms L1)
				 (rest-terms L2)))))))))


(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
	       (mul-terms (rest-terms L1) L2))))

(define (sub-terms L1 L2)
  (add-terms L1 (mul-terms L2 '((0 -1)))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let ((t2 (first-term L)))
      (adjoin-term 
	(make-term (+ (order t1) (order t2))
		   (mul (coeff t1) (coeff t2)))
	(mul-term-by-all-terms t1 (rest-terms L))))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-polynomial (variable p1)
		     (mul-terms (term-list p1)
				(term-list p2)))
    (error "Polys not in same var -- MUL-POLY"
	   (list p1 p2))))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
	  (t2 (first-term L2)))
      (if (> (order t2) (order t1))
	(list (the-empty-termlist) L1)
	(let ((new-c (div (coeff t1) (coeff t2)))
	      (new-o (- (order t1) (order t2))))
	  (let ((rest-of-result
		  (div-terms
		    (sub-terms
		      L1
		      (mul-terms L2 (adjoin-term (make-term new-o new-c) (the-empty-termlist))))
		    L2)))
	    (cons (adjoin-term (make-term new-o new-c) (car rest-of-result))
		  (cdr rest-of-result))))))))

(define (remainder-terms L1 L2)
  (cadr (div-terms L1 L2)))

(define (pseudo-remainder-terms L1 L2)
  (let ((factor (expt 
		  (coeff (first-term L2))
		  (+ 1 (- (order (first-term L1)) (order (first-term L2)))))))
    (let ((new-L1 (mul-terms L1 (list (list 0 factor)))))
      (remainder-terms new-L1 L2))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
    a
    (gcd-terms b (pseudo-remainder-terms a b))))

(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-polynomial (variable p1)
		     (gcd-terms (term-list p1) (term-list p2)))
    (error "Polys not in same var -- GCD-POLY"
	   (list p1 p2))))

(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define q1 (mul-poly p1 p2))
(define q2 (mul-poly p1 p3))

(gcd-poly q1 q2)
