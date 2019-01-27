(define (make-interval lower upper) (cons lower upper))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (percent i)
  (/ (width i) (center i)))

(define (mul-interval x y)
  (let ((xl (lower-bound x))
	(xu (upper-bound x))
	(yl (lower-bound y))
	(yu (upper-bound y))
	(xlp (>= (lower-bound x) 0))
	(xup (>= (upper-bound x) 0))
	(ylp (>= (lower-bound y) 0))
	(yup (>= (upper-bound y) 0)))
    (cond
      ((and xlp xup ylp yup) (make-interval (* xl yl) (* xu yu)))
      ((and xlp xup (not ylp) yup) (make-interval (* xu yl) (* xu yu)))
      ((and xlp xup (not ylp) (not yup)) (make-interval (* xu yl) (* xl yu)))
      ((and (not xlp) xup ylp yup) (make-interval (* xl yu) (* xu yu)))
      ((and (not xlp) xup (not ylp) yup) (make-interval (min (* xl yu) (* xu yl))
							(max (* xl yl) (* xu yu))))
      ((and (not xlp) xup (not ylp) (not yup)) (make-interval (* xu yl) (* xl yl)))
      ((and (not xlp) (not xup) ylp yup) (make-interval (* xl yu) (* xu yl)))
      ((and (not xlp) (not xup) (not ylp) yup) (make-interval (* xl yu) (* xl yl)))
      ((and (not xlp) (not xup) (not ylp) (not yup)) (make-interval (* xu yu) (* xl yl)))
      (else (error "Case not handled")))))

(define (close-enough? a b)
  (< (abs (- a b)) 0.0001))

(define (estimate-product-percent a b)
  (+ (percent a) (percent b)))

(let ((a (make-center-percent 100 0.001))
      (b (make-center-percent 100 0.002)))
  (close-enough?
    (percent (mul-interval a b))
    (estimate-product-percent a b)))
