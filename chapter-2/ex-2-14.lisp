(define (make-interval lower upper) (cons lower upper))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (width z)
  (- (upper-bound z) (lower-bound z)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define a (make-center-percent 100 0.01))
(define b (make-center-percent 1000 0.001))
