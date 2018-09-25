(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	 (else
	   (remainder (* base (expmod base (- exp 1) m))
		      m))))
	       

(define (carmichael-test n)
  (define (subtest a)
    (cond ((= a 0) 0)
	  ((= (expmod a n n) a) (subtest (- a 1)))
	  (else a)))
  (subtest (- n 1)))
