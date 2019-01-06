(define (average x y) (/ (+ x y) 2))

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
	next
	(try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y))))
	       1.0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (repeated (compose f f) (- n 1))))

(define (nth-root x n)
  (define (find-avg-damp-repetitions n)
    (define (test e r s)
      (if (< n (expt 2 e))
        r
        (test (+ e s) (+ r 1) (* s 2))))
    (test 2 1 1))
  (fixed-point ((repeated average-damp (find-avg-damp-repetitions n)) (lambda (y) (/ x (expt y (- n 1)))))
	       1.0))
