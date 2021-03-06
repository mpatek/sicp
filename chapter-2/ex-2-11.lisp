(define (make-interval lower upper) (cons lower upper))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

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
