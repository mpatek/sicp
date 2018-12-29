(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (if (predicate a) (term a) null-value)
	      (filtered-accumulate predicate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (true x) #t)
  (filtered-accumulate true combiner null-value term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next test-divisor)
    (cond ((= test-divisor 2) 3)
	  (else (+ test-divisor 2))))
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b )
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (sum-of-prime-squares a b)
  (define (square a) (* a a))
  (define (inc a) (+ a 1))
  (filtered-accumulate prime? + 0 square a inc b))

(define (product-of-relative-primes n)
  (define (relative-prime? x) (not (divides? (smallest-divisor x) n)))
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (filtered-accumulate relative-prime? * 1 identity 1 inc n))

