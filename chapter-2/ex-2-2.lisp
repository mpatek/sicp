(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b) (/ (+ a b) 2))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (make-point
    (average
      (x-point (start-segment s))
      (x-point (end-segment s)))
    (average
      (y-point (start-segment s))
      (y-point (end-segment s)))))


