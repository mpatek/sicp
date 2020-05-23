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

(define (embed-poly p v)
  (make-polynomial v (adjoin-term (make-term 0 p) (the-empty-termlist))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-polynomial (variable p1)
	       (add-terms (term-list p1)
			  (term-list p2)))
    (if (< (variable p1) (variable p2))
      (add-poly p1 (embed-poly p2 (variable p1)))
      (add-poly p2 (embed-poly p1 (variable p2))))))

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


(define (reduce-termlist-coeffs L)
  (let ((gcd-L (apply gcd (map coeff L))))
    (mul-terms L (list (list 0 (/ 1 gcd-L))))))

(define (gcd-terms a b)
  (reduce-termlist-coeffs
    (if (empty-termlist? b)
      a
      (gcd-terms b (pseudo-remainder-terms a b)))))

(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-polynomial (variable p1)
		     (gcd-terms (term-list p1) (term-list p2)))
    (error "Polys not in same var -- GCD-POLY"
	   (list p1 p2))))

(define (constant-termlist x)
  (list (list 0 x)))

(define (reduce-terms a b)
  (let ((gcd-ab (gcd-terms a b)))
    (let ((factor (constant-termlist (expt (coeff (first-term gcd-ab))
				      (+ 1 (- 
					     (max (order (first-term a))
						  (order (first-term b)))
					     (order (first-term gcd-ab))))))))
      (let (
	    (aa (car (div-terms (mul-terms a factor) gcd-ab)))
	    (bb (car (div-terms (mul-terms b factor) gcd-ab))))
	(let ((gcd-coeff (constant-termlist (gcd
					 (apply gcd (map coeff aa))
					 (apply gcd (map coeff bb))))))
	  (list (car (div-terms aa gcd-coeff))
		(car (div-terms bb gcd-coeff))))))))

(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (let ((reduced-terms (reduce-terms (term-list p1) (term-list p2))))
      (list 
	(make-polynomial (variable p1) (car reduced-terms))
	(make-polynomial (variable p1) (cadr reduced-terms))))
    (error "Polys not in same var -- REDUCE-POLY"
	   (list p1 p2))))

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

; (define (reduce n d) (apply-generic 'reduce n d))
(define (reduce n d) (reduce-poly n d))

(define (make-rational n d) (reduce n d))
(define (numer r) (car r))
(define (denom r) (cadr r))
(define (add-rational r1 r2)
  (make-rational
    (add-poly 
      (mul-poly (numer r1) (denom r2))
      (mul-poly (numer r2) (denom r1)))
    (mul-poly (denom r1) (denom r2))))

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(add-rational rf1 rf2)
