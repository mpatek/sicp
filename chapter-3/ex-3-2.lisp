(define (make-monitored f)
  (let ((c 0))
    (lambda (arg)
      (cond
	((eq? arg 'how-many-calls?) c)
	((eq? arg 'reset-count) (set! c 0))
	(else (set! c (+ c 1)) (f arg))))))
