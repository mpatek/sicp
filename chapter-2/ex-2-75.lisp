(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* (cos a) r))
	  ((eq? op 'imag-part) (* (sin a) r))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	    (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
