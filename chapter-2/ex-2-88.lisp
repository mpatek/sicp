(put 'negate 'scheme-number (lambda (x) (- x)))
(put 'negate 'complex (lambda (z) (make-complex (negate (real-part z)) (negate (imag-part z)))))
(put 'negate 'rational (lambda (r) (make-rational (negate (numer r)) (denom r))))
(put 'negate 'polynomial
     (let ((negate-term (lambda (t) (make-term (order term) (negate (coeff term))))))
       (lambda (p)
	 (make-poly (variable p)
		    (map negate-term (term-list p))))))

(define (negate x)
  (apply-generic 'negate x))

(put 'sub '(polynomial polynomial)
     (lambda (p1 p2)
       ((get 'add '(polynomial polynomial))
	p1 (negate p2))))
