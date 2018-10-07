(define (miller-rabin-test n)
  (define (m-r-helper n)
    (define (try-it a)
      (= 1 (expmod-check a (- n 1) n)))
    (try-it (+ 1 (random (- n 1)))))
  (cond 
    ((<= n 3) #t)
    ((even? n) #f)
    (else (m-r-helper n))))

(define (expmod-check base exp m)
  (define (helper-1 x m)
    (helper-2 x (square x) m))
  (define (helper-2 x s m)
    (helper-3 x (remainder s m) m))
  (define (helper-3 x r m)
    (cond ((= x 1) r)       ; trivial case
	  ((= x (- m 1)) r) ; trivial case
	  ((= r 1) 0)       ; definitely not prime
	  (else r)))
  (cond ((= exp 0) 1)
	((even? exp)
	 (helper-1 (expmod-check base (/ exp 2) m) m))
	(else
	   (remainder (* base (expmod-check base (- exp 1) m))
		      m))))
