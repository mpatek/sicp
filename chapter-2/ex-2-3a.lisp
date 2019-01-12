(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left-rect r) (car r))
(define (bottom-right-rect r) (cdr r))
(define (width-rect r) (abs (-
			 (x-point (bottom-right-rect r))
			 (x-point (top-left-rect r)))))
(define (height-rect r) (abs (-
			 (y-point (top-left-rect r))
			 (y-point (bottom-right-rect r)))))
(define (area-rect r) (* (width-rect r) (height-rect r)))
(define (perimiter-rect r) (+ (* 2 (width-rect r)) (* 2 (height-rect r))))
