(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (inc x) (+ x 1))
(define (iden x) x)
(define (factorial x)
  (product iden 2 inc x))

(define (wallis x)
  (define (term a)
    (if (even? a)
      (/ (+ a 2) (+ a 1))
      (/ (+ a 1) (+ a 2))))
  (* 4.0 (product term 1 inc x)))
