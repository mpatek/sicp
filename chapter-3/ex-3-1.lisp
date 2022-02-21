(define (make-accumulator x)
  (lambda (inc)
    (set! x (+ x inc))
    x))
