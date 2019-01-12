(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rect top-left width height)
  (cons top-left (cons width height)))

(define (top-left-rect r) (car r))
(define (width-rect r) (car (cdr r)))
(define (height-rect r) (cdr (cdr r)))
(define (area-rect r) (* (width-rect r) (height-rect r)))
(define (perimiter-rect r) (+ (* 2 (width-rect r)) (* 2 (height-rect r))))
