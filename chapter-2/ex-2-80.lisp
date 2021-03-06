(define (install-iszero-operation)
  (define (=zero? x) (apply-generic '=zero? x))
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put '=zero? '(complex) (lambda (x y) (and (= (real-part x) 0) (= (imag-part x) 0)))))
