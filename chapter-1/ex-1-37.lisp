(define (cont-frac n d k)
  (define (helper n d i k)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (helper n d (+ i 1) k)))))
  (helper n d 1 k))


(define (cont-frac-iter n d k)
  (define (helper result n d i)
    (if (= i 0)
      result
      (helper 
	(/ (n i) (+ (d i) result))
	n
	d
	(- i 1))))
  (helper 0 n d k))


(define (golden-ratio k)
  (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))

(define (golden-ratio-iter k)
  (/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) k)))
