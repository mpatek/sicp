(define (deep-reverse items)
  (define (deep-reverse-iter items accumulator)
    (cond ((null? items) accumulator)
	  (else (deep-reverse-iter
		  (cdr items)
		  (cons
		    (cond ((pair? (car items)) (deep-reverse-iter (car items) (list)))
			  (else (car items)))
		    accumulator)))))
  (deep-reverse-iter items (list)))
