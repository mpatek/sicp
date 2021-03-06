(define (make-monitored f)
  (define counter 0)
  (lambda (x)
    (cond ((eq? x 'reset-counter) (set! counter 0))
	  ((eq? x 'how-many-calls?) counter)
	  (else (set! counter (+ counter 1)) (f x)))))
