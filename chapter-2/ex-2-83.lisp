(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (make-integer contents)
  (attach-tag 'integer contents))

(define (make-rational numer denom)
  (attach-tag 'rational (list numer denom)))

(define (make-rational-from-integer contents)
  (make-rational numer 1))

(define (make-real contents)
  (attach-tag 'real contents))

(define (numer rat) (car rat))
(define (denom rat) (cadr rat))

(define (make-real-from-rational rat)
  (make-real (/ (numer rat) (denom rat))))

(define (make-complex real-part imag-part)
  (attach-tag 'complex (list real-part imag-part)))

(define (make-complex-from-real contents)
  (make-complex contents 0))

(define (put op type-tag item)
  (display "put not implemented yet!"))

(put 'raise '(integer) make-rational-from-integer)
(put 'raise '(rational) make-real-from-rational)
(put 'raise '(real) make-complex-from-real)

(define (raise x) (apply-generic 'raise x))
