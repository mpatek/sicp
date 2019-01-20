(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f ((
			       (lambda (f) (lambda (x) x))
			       f) x)))))

(define two
  (lambda (f) (lambda (x) (f ((
			       (lambda (f) (lambda (x) (f ((
							    (lambda (f) (lambda (x) x))
							    f) x))))
			       f) x)))))

(define (plus g h)
  (lambda (f) (lambda (x) ((g f) ((h f) x)))))

(define (incr x) (+ x 1))

(= (((plus one two) incr) 0) 3)
