(define (get-coercion-type types)
  (define (can-coerce? src-type dest-type)
    (if (= src-type dest-type)
      true
      (get-coercion src-type dest-type)))
  (define (can-coerce-multi? src-types dest-type)
    (cond
      ((null? src-types) true)
      ((can-coerce? (car src-types) dest-type) (can-coerce-multi? (cdr src-types) dest-type))
      (else false)))
  (define (try-types dest-types)
    (cond
      ((null? dest-types) (error "No coercion for these types" types))
      ((can-coerce-multi? types (car dest-types)) (car dest-types))
      (else (try-types (cdr dest-types)))))
  (try-types types))

(define (coerce-args args)
  (define (coerce-arg coercion-type arg)
    (let ((arg-type (type-tag arg)))
      (if (= arg-type coercion-type)
	arg
	((get-coercion arg-type coercion-type) arg))))
  (if (= (length args) 0)
    args
    (let ((coercion-type (get-coercion-type (map type-tag args))))
      (map (lambda (arg) (coerce-arg coercion-type arg)) args))))

(define (apply-generic op . args)
  (let ((coerced-args (coerce-args args)))
    (let ((type-tags (map type-tag coerced-args)))
      (let ((proc (get op type-tags)))
	(if proc
	  (apply proc (map contents coerced-args))
	  (error "No method for these types"
		 (list op type-tags)))))))
