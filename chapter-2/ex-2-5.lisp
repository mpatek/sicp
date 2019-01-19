(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (n-factors x f)
  (define (helper y c)
    (if (> (modulo y f) 0)
      c
      (helper (/ y f) (+ c 1))))
  (helper x 0))

(define (car z)
  (n-factors z 2))

(define (cdr z)
  (n-factors z 3))
