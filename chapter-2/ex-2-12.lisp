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
