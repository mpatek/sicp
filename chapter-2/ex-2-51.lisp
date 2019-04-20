(define (below painter1 painter2)
  (let ((paint-below
	  (transform-painter painter1
			     (make-vect 0.0 0.0)
			     (make-vect 1.0 0.0)
			     (make-vect 0.0 0.5)))
	(paint-above
	  (transform-painter painter2
			     (make-vect 0.0 0.5)
			     (make-vect 1.0 0.5)
			     (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-below frame)
      (paint-above frame))))

(define (below-rotate painter1 painter2)
  (rotate90 (beside painter1 painter2)))
