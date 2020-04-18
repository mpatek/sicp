(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (make-integer contents)
  (attach-tag 'integer contents))

(define (make-real contents)
  (attach-tag 'real contents))

(define (make-complex real-part imag-part)
  (attach-tag 'complex (list real-part imag-part)))

(define (make-real-from-integer contents)
  (make-real contents))

(define (make-complex-from-real contents)
  (make-complex contents 0))

(define (put op type-tag item)
  (display "put not implemented yet!"))

(define (raise x) (apply-generic 'raise x))
(put 'raise '(integer) make-real-from-integer)
(put 'raise '(real) make-complex-from-real)

(define (make-real-from-complex contents)
  (make-real (real-part contents)))

(define (make-integer-from-real contents)
  (make-integer contents))

(define (project x) (apply-generic 'project x))
(put 'project '(complex) make-real-from-complex)
(put 'project '(real) make-integer-from-real)

(define (equ? x y) (apply-generic 'equ? x y))
(put 'equ? '(real real) (lambda (x y) (= x y)))
(put 'equ? '(integer integer) (lambda (x y) (= x y)))
(put 'equ? '(complex complex) (lambda (x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))))

(define (drop arg)
  (if (and
	(get 'project (type-tag arg))
	(equ? (raise (project arg)) arg))
    (drop (project arg))
    arg))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(if (and (= (length args) 2) (not (= (car type-tags) (cadr type-tags))))
	  (let ((a1 (car args)) (a2 (cadr args)))
	    (let ((d1 (drop a1)) (d2 (drop a2)))
	      (if (or
		    (not (eq? (type-tag a1) (type-tag d1)))
		    (not (eq? (type-tag a2) (type-tag d2))))
		(apply-generic op d1 d2)
		(error "No method for these types" (list op type-tags)))))
	  (error "No method for these types" (list op type-tags)))))))
