(define (make-interval upper lower) (cons upper lower))

(define (upper-bound interval) (car interval))

(define (lower-bound interval) (cdr interval))
