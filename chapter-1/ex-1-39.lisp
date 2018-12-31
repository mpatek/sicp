(define (cont-frac n d k)
  (define (helper n d i k)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (helper n d (+ i 1) k)))))
  (helper n d 1 k))

(define (tan-cf x k)
  (cont-frac
    (lambda (i)
      (if (= i 1) x (- (* x x))))
    (lambda (i) (- (* 2 i) 1))
    k))
