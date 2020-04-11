(define (can-raise? source target)
  (cond
    ((= (type-tag source) (type-tag target)) true)
    ((get 'raise (type-tag source)) (can-raise? (raise source) target))
    (else false)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(if (and (= (length args) 2) (not (= (car type-tags) (cadr type-tags))))
	  (let ((a1 (car args))
		(a2 (cadr args)))
	    (cond
	      ((can-raise? a1 a2) (apply-generic op (raise a1) a2))
	      ((can-raise? a2 a1) (apply-generic op a1 (raise a2)))
	      (else (error "No method for these types" (list op type-tags)))))
	  (error "No method for these types" (list op type-tags)))))))
