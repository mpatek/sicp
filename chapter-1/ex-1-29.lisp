(define (simpsons f a b n)
  (define (helper-1 n)
    (helper-2 (/ (- b a) n) n))
  (define (helper-2 h n)
    (define (coef k)
      (cond ((= k 0) 1)
	    ((= k n) 1)
	    ((even? k) 2)
	    (else 4)))
    (define (term k)
      (* (coef k) (f (+ a (* k h)))))
    (* (/ h 3) 
       (sum term 0 inc n)))
  (helper-1 (cond ((even? n) n) (else (inc n)))))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc x)
  (+ x 1))
