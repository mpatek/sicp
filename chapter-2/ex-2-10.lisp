(define (make-interval lower upper) (cons lower upper))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (width z)
  (- (upper-bound z) (lower-bound z)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (width y) 0)
    (error "Division by a zero-width interval is not defined.")
    (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))
