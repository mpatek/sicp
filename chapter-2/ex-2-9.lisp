(define (make-interval lower upper) (cons lower upper))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (width z)
  (- (upper-bound z) (lower-bound z)))

(define a (make-interval 1 2))
(define b (make-interval 3 4))

(define (assert p msg)
  (newline)
  (if p
    (display "PASS ")
    (display "FAIL "))
  (display msg)
  p)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y)))))
   
(assert 
  (= (+ (width a) (width b))
     (width (add-interval a b)))
  "width of the sum equals sum of the widths.")
(assert
  (= (- (width a) (width b))
     (width (sub-interval a b)))
  "width of the difference equals difference of the widths.")
(assert
  (not (= (* (width a) (width b))
     (width (mul-interval a b))))
  "width of the product does not equal product of the widths.")
(assert
  (not (= (/ (width a) (width b))
     (width (div-interval a b))))
  "width of the quotient does not equal quotient of the widths.")
