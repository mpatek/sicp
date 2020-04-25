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

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
	       (div-terms (term-list p1) 
			  (term-list p2)))
    (error "Polys not in same var -- DIV-POLY"
	   (list p1 p2))))
